#Migrating from Flask to Scotty
###Or: Rewriting a Python web application in Haskell

A few months ago, I wrote a program to scale recipes based on the given serving size and the desired serving size.

Initially, it was a command line application that I wrote in Haskell. It parsed a text file containing the recipe that was formatted in a specific way and would either display the output on stdout or write it to an optionally provided file.

I also wanted to make a simple web front-end, so that end users would be able to use the application with minimal friction. I've used Haskell for a little while now, but at the time I hadn't touched any of its web frameworks (the main ones I'm aware of being <a href="www.yesodweb.com">Yesod</a>, <a href="http://snapframework.com/">Snap</a>, <a href="http://happstack.com/">Happstack</a>, and <a href="https://github.com/scotty-web/scotty/wiki">Scotty</a>). To get something up and running fast, I opted to use Python and Flask.

I was able to write the application (Flask, CSS, Jinja2 template, and JavaScript) in a short period of time and, with everything working fairly well, I put the application on the backburner.

In the back of my head, I had been meaning to go back and rewrite the app in Haskell. The Python implementation was functional, but there were some hacks involved that I wasn't entirely happy with. 

On the front end, the user would enter the recipe and click a submit button. When the button was clicked, the recipe was bundled into a JSON object and passed to the Flask application via AJAX. From there, it was parsed and written to a file using the specific formatting required by the Haskell application. A subprocess was then spawned that called the Haskell recipe scaler with the file. The recipe would be scaled and then piped back to the Flask app using the subprocess' communicate() method. The resulting recipe string was parsed into a dictionary and passed as JSON back to the frontend to be inserted into the page.

This worked, but I knew it could be better. By rewriting the application completely in Haskell, I could:
<ol>
<li>Avoid spawning a subprocess to actually do the conversion.</li>
<li>Use the JSON directly from the AJAX request and bypass writting the recipe formatted file.</li>
<li>Put Haskell's incredible type system to use and benefit from its type safety.</li>
<li>Learn how to use one of Haskell's web frameworks.</li>
</ol>

I did a little research and settled on Scotty as my framework of choice for this rewrite. One of the things I like most about Flask is how light-weight it is. It provides the tools needed to get the job done and it gets out of the way. Scotty, inspired by Ruby's Sinatra, seems to have the same philosophy. I also wanted to use some sort of HTML templating library that would allow me to make dynamic pages. <a href="http://jaspervdj.be/blaze/">blaze-html</a> "embeds HTML templates in Haskell code" and works well with Scotty, so I decided to give it a try.

Scotty's <a href="http://hackage.haskell.org/package/scotty-0.4.0">hackage page</a>, <a href="http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html">this</a> scotty tutorial, and Ollie Charles' 24 Days of Hackage post on <a href="https://ocharles.org.uk/blog/posts/2013-12-05-24-days-of-hackage-scotty.html">scotty</a> provided a great foundation for getting my application up and running. 

The routing is handled in the main function:
<pre class="pretty-print">
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ S.html . renderHtml $ homePage
    get "/about" $ S.html . renderHtml $ aboutPage
    post "/recipe_scale" handleRecipe
</pre>
This runs the application on port 3000 using the warp server. The middleware line is there to handle requests for static files (from the static directory). The next three lines handle the application's routing. Get requests for root and /about render the HTML for the appropriate pages, and post requests to /recipe_scale from the AJAX submission are sent to the handleRecipe function.



Using: Scotty and Blaze.Html

Problems:
    Aeson - poorly typed json coming in wouldn't type check; took a while to debug (floats as strings)

   
Links: 
    https://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html
    http://the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
