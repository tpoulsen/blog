Pseudo-Pointillism
=====
Among my favorite art styles are <a href="http://www.wikiart.org/en/paintings-by-style/pointillism">pointillism</a> and <a href="http://www.wikiart.org/en/paintings-by-style/cubism">cubism</a>. By no means am I an art critic or expert, but I think my attraction to these styles stems from the way they (respectively) deconstruct the colors and shapes of their subjects.

As a personal exerciese, I wanted to write a program to take images and reproduce them in a pseudo-pointillist style. Prior to this, I hadn't had any experience programtically manipulating images. 

The "problem" I set for myself: <em>Given an image, reproduce it using points in such a way that it is still recognizable as its source image.</em>   

<img src="results/mona_lisa.png"/>
<br>
<sub>Pseudo-pointillist <a href="http://www.wikiart.org/en/leonardo-da-vinci/mona-lisa">Mona Lisa</a></sub>

Approach
=====
In coming up with the solution to this challenge, I had several things in mind that I wanted to use:
<ul>
    <li>D3 - I use <a href="http://d3js.org/">D3</a> for creating graphs and other data visualizations. I ultimately wanted to turn this into a small web application, so using D3 to render the resulting "pointillist" image seemed like a logical approach.</li>
    <li>Python - The packages available for python make it possible to do <a href="http://xkcd.com/353/">anything<a>. I use <a href="http://pandas.pydata.org/">pandas</a> and <a href="http://flask.pocoo.org/">flask</a> a lot for data analysis and web development respectively, so using python to handle the pixel/color data for the images and as a backend for the web application made perfect sense.</li>
</ul>

At this point, I needed to find a way to extract the pixel data from the source images. Google and stackoverflow pointed me to <a href="https://pypi.python.org/pypi/Pillow">pillow</a>, a <a href="https://pillow.readthedocs.org/en/latest/">"'friendly' fork of the Python Imaging Library"</a>. Given a filename, getting the pixel data from the image using pillow was straightforward<sup><a href="#footnotes">1</a></sup>.



````Python
# Open the image and return an image object.

In[0]: im = Image.open("./example_picture.jpg")

# List comprehension that calls im.getpixel() 
#   -- the method that gets the RGB information for a pixel at location (X, Y) -- 
#   for all of the pixels in the image. Returns a list of (R, G, B) tuples  by taking 
#   the (X) and (Y) values from im.size() -- the method that returns the dimensions 
#   of the image -- and looping through all of their combinations.

In[1]: pix = [[im.getpixel((x,y)) for x in range(im.size[0])] \
             for y in range(im.size[1])]
````

The actual type of pix as returned by the list comprehension is a list of lists of tuples ( [[(R, G, B)]] ). Each row of pixels is its own list, so subsetting <strong>pix</strong> with (x, y) coordinates, <em>e.g.</em> <strong>pix[x][y]</strong> returns the (R, G, B) value for a given pixel.

Next, I needed to take the RGB values and format them to use with D3. Putting the list-of-lists-of-tuples into a pandas DataFrame made it easier to structure the elements and get them in the format I wanted.

To plot the data in D3, I wanted to make each pixel a javascript object containing its x and y coordinates and its color information. 

````Python
# To reduce the number of points needed to reproduce the image, I set a max height for the results.
In[3]: pixels_high = 80 
# I then set a 'skip' value that would take every X pixels from the image based on its height.
In[4]: skip = round(im.size[1]/pixels_high)
# Constructing the Data Frame.
In[5]: pix_frame = pd.DataFrame(pix)
# List comprehension that selects the appropriate pixels based on the skip value and
#   formats the data as JSON for plotting in D3. 
In[6]: colors = [{"x": x, "y": y, "color": "rgba({0}, {1}, {2}, 0.75)".\
                format(pix_frame[x][y][0], pix_frame[x][y][1], pix_frame[x][y][2])} \
                for y in pix_frame.index \
                for x in pix_frame.columns \
                if y % skip == 0 and x % skip == 0]
````
The result of this chunk of code is a list of dictionaries [{"x": X, "y": Y, "color": "rgb(R, G, B, A)"}, ...] where X and Y are the coordinates of the point and color is the RGB value with an added alpha component for a little transparency. Setting the skip value results in some separation between the points when plotting them, so that 1) there are fewer points to plot overall and 2) the points are distinct. Formatting the data like this allows it to be passed to D3 as JSON for plotting.

With the basic code to get and format the pixel data from the images working, I wrote a <a href="#pointillism_object">class</a> to streamline the data-aquisition process. With the class' built-in methods, the flask application can easily get the pixel data from any given image in just a few lines of code.

<img src="results/starry_night.png"/>
<br>
<sub>Pseudo-pointillist <a href="http://www.wikiart.org/en/vincent-van-gogh/the-starry-night-1889">The Starry Night</a></sub>

The <a href="#flask_app">flask application</a> itself is relatively simple. It has a landing page ("/") that renders a template (containing the HTML and javascript that handles the actual visualization), a callable address ("/new_picture") that handles getting a new picture when the "new picture" event handler is triggered, and a couple convenience methods that pick a random image from the "images" directory and make calls to the PointillismImage class. 

The <a href="#template">template</a> sets up a basic web page. Flask uses the Jinja2 templating engine, so there is a trick I'm using here worth mentioning. Where things are enclosed in doubly curly braces -- {{}} -- these are replaced by variables passed in by the flask application. I'm using this feature here to pass in the dataset and aspect ratio for the intial image.

The main draw of D3, for me, is the ease with which one can model data, and adjust the models when the data change. A brief explanation of how it works: you can make selections, bind data to those selections, and draw elements based on those data. 

````Javascript
// Set height and width of the final image based on the height
// of the window and the aspect ratio of the image.
var aspect = {{ aspect }} // Substituted with the value provided by flask.
var h = window.innerHeight - 100;
var w = aspect * h;

// Select the body and append an svg element with height h and width w.
// Save the selection as 'svg'.
var svg = d3.select("body")
    .append("svg")
    .attr({
        height: h,
        width: w
    });

// Describes scales that map an input domain to an output range.
// Ensures that the (x, y) coordinates are scaled to fit in the svg. 
var xScale = d3.scale.linear()
                    .domain([0, d3.max(dataset, function(d) {return d.x;})])
                    .range([padding, w - padding]);
var yScale = d3.scale.linear()
                    .domain([0, d3.max(dataset, function(d) {return d.y;})])
                    .range([padding, h - padding]);
var rScale = d3.scale.linear()
                    .domain([0, d3.max(dataset, function(d) {return d.y;})])
                    .range([4, 4]);

// In svg, select all "circle" elements,
// Note that at this point, they don't exist, but they will soon.
var circles = svg.selectAll("circle")
    .data(dataset)                              // Bind dataset to the selection.
    .enter()                                    // Call the enter method.
    .append("circle")                           // Append circles to the svg for the datapoints in dataset.
    ;

// Describes the circles.
circles.attr({
        cx: function(d) {return xScale(d.x);},  // X coordinate is the x property of the object scaled.
        cy: function(d) {return yScale(d.y);},  // Y coordinate is the y property of the object scaled.
        r: 4,                                   // Radius is hardcoded to 4 pixels for now.
        fill: function(d) {return d.color}      // Fill the circle using its color property.
    });
````
There is additional code (included below) that handles updating the points when the data change, as well as how to handle adding new points and removing old points when the number of datapoints changes. There's also a function that initiates an AJAX call to the backend that triggers loading data for a new image.

Results
=====
Examples of the Mona Lisa and The Starry Night are included above. The other test images I used were:

 Georges Seurat's <a href="http://www.wikiart.org/en/georges-seurat/sunday-afternoon-on-the-island-of-la-grande-jatte-1886">Sunday Afternoon on the Island of La Grande Jatte</a>:

<img src="results/sunday_afternoon.png"/>

<a href="http://www.wikiart.org/en/georges-seurat/alfalfa-st-denis-1886">Alfalfa, St. Denis</a>, also by Seurat:

<img src="results/alfalfa.png"/>

<a href="http://www.wikiart.org/en/edvard-munch/the-scream-1893">The Scream</a> by Edvard Munch:

<img src="results/scream.png">

<a href="http://www.wikiart.org/en/georges-braque/the-park-at-carri%C3%A8res-saint-denis-1909">The Park at Carrières-Saint-Denis</a> by Georges Braque

<img src="results/the_park.png">

Overall I'm pleased with the way it turned out, and it was a fun way to experiment with working with images. There are a few things I'm planning to add (<em>e.g.</em> making the point radius and the pixel heights of the images variable and changeable by the user, loading some information about the art beside it) before putting it up as an application.

The code itself is included below and is also available on <a href="https://github.com/tpoulsen/PseudoPointillism">GitHub</a>, where it can be downloaded. The GitHub repo includes the prerequisites needed to get everything up and running as well as the example images that I used.
 
Footnotes
=====
<div id="footnotes">
<sup>1</sup> Getting it to work with jpegs on OS X required installing libjpeg (via <a href="http://brew.sh/">Homebrew</a>) and setting up the XCode command lines tools <em>xcode-select --install</em>. With those requirements satisfied, <em>pip install pillow</em> got pillow to install properly. 
</div>

Code
=====
<div id="pointillism_object"><h4>Pointillism Object</h4></div>
````Python
import pandas as pd
from PIL import Image

class PointillismImage(object):
    """
    Opens an image and provides accessors for aspect ratio and
    JSON formatted pixel data.

    """
    def __init__(self, f_name):
        """
        Initializes with provided filename.

        """
        self.f_name = f_name
        self.im = self.open_image()
        self.pixel_matrix = self.set_pixel_data()

    def open_image(self):
        """
        Opens image and sets the Image object to self.im

        """
        return Image.open(self.f_name)

    def set_pixel_data(self):
        """
        Gets pixel colors (R,G,B) for all (X, Y)'s. Sets self.pixel_matrix
        with the resulting Data Frame.

        """
        pix = [[self.im.getpixel((x, y)) for x in range(self.im.size[0])] \
            for y in range(self.im.size[1])]
        pix_frame = pd.DataFrame(pix)
        return pix_frame

    def get_pixel_json(self, height):
        """
        Uses height and sets skip to determine which pixels to take,
        then formats the the points needed to plot the image in a list
        of dicts that will be parseable from D3.

        """
        skip = round(self.im.size[1]/height)
        colors = [{"x": x, "y": y, "color": "rgba({0}, {1}, {2}, 0.75)".\
                format(self.pixel_matrix[x][y][0], self.pixel_matrix[x][y][1], \
                self.pixel_matrix[x][y][2])} for y in self.pixel_matrix.index \
                for x in self.pixel_matrix.columns if y % skip == 0 \
                and x % skip == 0]
        return colors
   
    def get_aspect(self):
        """
        Floating point aspect ratio of image.
        """
        return self.im.size[0] / float(self.im.size[1])
````

<div id="flask_app"><h4>Flask App</h4></div>
````Python
from flask import Flask, request, render_template, jsonify
from random import randrange
import os

import pointillism_image as pointillism

app = Flask(__name__)

@app.route("/", methods=["GET", "POST"])
def main():
    """
    Entry point. Selects a random picture from './images', and returns the
    pixel and aspect information to the application on loading.

    """
    image_data = image_handler()
    return render_template("layout.html", aspect=image_data[0], dataset=image_data[1])

@app.route("/new_picture", methods=["GET", "POST"])
def generate_picture_data():
    """
    Called by click handler in javascript to draw a new image.
    Returns pixel and aspect data as JSON.

    """
    image_data = image_handler()
    return jsonify({"aspect": image_data[0], "dataset": image_data[1]})

def image_handler():
    """
    Selects random picture, opens it and returns pixel and aspect 
    data.
    """
    picture = select_picture("images")
    image = pointillism.PointillismImage(picture)
    aspect = image.get_aspect()
    dataset = image.get_pixel_json(70)
    return (aspect, dataset)

def select_picture(pic_dir):
    """
    Selects random image from images directory.

    """
    pics = os.listdir(pic_dir)
    return "{0}/{1}".format(pic_dir, pics[randrange(0, len(pics))])

if __name__ == "__main__":
    app.debug = True
    app.run(port=8080)
    
````

<div id="template"><h4>Template</h4></div>
````HTML
<!DOCTYPE HTML>
<html>
    <head>
        <meta charset="utf-8">
        <script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
        <title>Pointilism!</title>
    </head>
    <body>
        <h4>Random picture time!</h4>

        <script type="text/javascript">
        /*
        Variables for page layout and data.
        */
        var aspect = {{ aspect }}
        var h = window.innerHeight - 100;
        var w = aspect * h;
        var padding = 10;
        var dataset = {{ dataset | tojson | e}};

        /*
        Scales for positioning of the points.
        */
        var xScale = d3.scale.linear()
                            .domain([0, d3.max(dataset, function(d) {return d.x;})])
                            .range([padding, w - padding]);
        var yScale = d3.scale.linear()
                            .domain([0, d3.max(dataset, function(d) {return d.y;})])
                            .range([padding, h - padding]);
        var rScale = d3.scale.linear()
                            .domain([0, d3.max(dataset, function(d) {return d.y;})])
                            .range([4, 4]);                

        var svg = d3.select("body")
            .append("svg")
            .attr({
                height: h,
                width: w
            });
        var circles = svg.selectAll("circle")
            .data(dataset)
            .enter()
            .append("circle")
            ;

        circles.attr({
            cx: function(d) {return xScale(d.x);},
            cy: function(d) {return yScale(d.y);},
            r: 4,
            //r: function(d) {return rScale(d.x);},
            fill: function(d) {return d.color}
        });

        d3.select("body").select("h4")
            .on("click", function(){
                new_image();
            });

        var update_image = function(newDataset) {
                h = window.innerHeight - 100;
                console.log(h);
                w = aspect * h;

                xScale = d3.scale.linear()
                            .domain([0, d3.max(newDataset, function(d) {return d.x;})])
                            .range([padding, w - padding]);
                yScale = d3.scale.linear()
                            .domain([0, d3.max(newDataset, function(d) {return d.y;})])
                            .range([padding, h - padding]);
                rScale = d3.scale.linear()
                            .domain([0, d3.max(newDataset, function(d) {return d.y;})])
                            .range([4, 4]);                
                var circles = svg.selectAll("circle")
                    .data(newDataset);

                circles.enter()
                    .append("circle")
                    .transition()
                    //.duration(500)
                    .attr({
                        cx: function(d) {return xScale(d.x);},
                        cy: function(d) {return yScale(d.y);},
                        r: 4,
                        //r: function(d) {return rScale(d.y);},
                        fill: function(d) { return d.color; }
                    });

                circles.transition()
                    .duration(500)
                    .attr({
                        cx: function(d) {return xScale(d.x);},
                        cy: function(d) {return yScale(d.y);},
                        r: 6,
                        fill: function(d) { return d.color; },
                    })
                    .each("end", function() {
                        d3.select(this)
                            .transition()
                            .duration(500)
                            .attr({
                                r: 4
                                //r: function(d) {return rScale(d.y);}
                            })
                    });

                circles.exit()
                    .transition()
                    .duration(500)
                    .attr("x", w)
                    .remove();
            };
            /*
            [{x: _, y: _, color: _}, ...]
            */
            var new_image = function() {
                d3.json("{{ url_for('generate_picture_data') }} ", function(error, data) {
                    if (data) { 
                        var dataset = data.dataset;
                        aspect = data.aspect;
                        update_image(dataset);
                    } else {
                        console.log(error);
                    };
                });
            };

        </script>
    </body>
</html>
````
