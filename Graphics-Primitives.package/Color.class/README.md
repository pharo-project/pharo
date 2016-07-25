This class represents abstract color, regardless of the depth of bitmap it will be shown in.  At the very last moment a Color is converted to a pixelValue that depends on the depth of the actual Bitmap inside the Form it will be used with.  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.  (See comment in BitBlt.)  To change the depth of the Display and set how many colors you can see, execute: (Display newDepth: 8).  (See comment in DisplayMedium)
	Color is represented as the amount of light in red, green, and blue.  White is (1.0, 1.0, 1.0) and black is (0, 0, 0).  Pure red is (1.0, 0, 0).  These colors are "additive".  Think of Color's instance variables as:
	r	amount of red, a Float between 0.0 and 1.0.
	g	amount of green, a Float between 0.0 and 1.0.
	b	amount of blue, a Float between 0.0 and 1.0.
(But, in fact, the three are encoded as values from 0 to 1023 and combined in a single integer, rgb.  The user does not need to know this.)
	Many colors are named.  You find a color by name by sending a message to class Color, for example (Color lightBlue).  Also, (Color red: 0.2 green: 0.6 blue: 1.0) or (Color r: 0.2 g: 0.6 b: 1.0) creates a color. (see below)
	A color is essentially immutable.  Once you set red, green, and blue, you cannot change them.  Instead, create a new Color and use it.
	Applications such as contour maps and bar graphs will want to display one of a set of shades based on a number.  Convert the range of this number to an integer from 1 to N.  Then call (Color green lightShades: N) to get an Array of colors from white to green.  Use the Array messages at:, atPin:, or atWrap: to pull out the correct color from the array.  atPin: gives the first (or last) color if the index is out of range.  atWrap: wraps around to the other end if the index is out of range.
	Here are some fun things to run in when your screen has color:
		Pen new mandala: 30 diameter: Display height-100.
		Pen new web  "Draw with the mouse, opt-click to end"
		Display fillWhite.  Pen new hilberts: 5.
		Form toothpaste: 30  "Draw with mouse, opt-click to end"
You might also want to try the comment in
	Form>class>examples>tinyText...


Messages:
	mixed: proportion with: aColor	Answer this color mixed with the given color additively. The proportion, a number between 0.0 and 1.0, determines what what fraction of the receiver to use in the mix.

	+ 	add two colors
	- 	subtract two colors
	*	multiply the values of r, g, b by a number or an Array of factors.  ((Color named: #white) * 0.3) gives a darkish gray.  (aColor * #(0 0 0.9)) gives a color with slightly less blue.
	/	divide a color by a factor or an array of three factors.

	errorForDepth: d     How close the nearest color at this depth is to this abstract color.  Sum of the squares of the RGB differences, square rooted and normalized to 1.0.  Multiply by 100 to get percent.

	hue			Returns the hue of the color. On a wheel from 0 to 360 with pure red at 0 and again at 360.
	saturation	Returns the saturation of the color.  0.0 to 1.0
	brightness	Returns the brightness of the color.  0.0 to 1.0

	name    Look to see if this Color has a name.
	display	Show a swatch of this color tracking the cursor.

	lightShades: thisMany		An array of thisMany colors from white to the receiver. 
	darkShades: thisMany		An array of thisMany colors from black to the receiver.  Array is of length num.
	mix: color2 shades: thisMany		An array of thisMany colors from the receiver to color2.
	wheel: thisMany			An array of thisMany colors around the color wheel starting and ending at the receiver.

	pixelValueForDepth: d    Returns the bits that appear be in a Bitmap of this depth for this color.  Represents the nearest available color at this depth.  Normal users do not need to know which pixelValue is used for which color. 

Messages to Class Color.
	red: r green: g blue: b		Return a color with the given r, g, and b components.
	r: g: b:		Same as above, for fast typing.

 	hue: h saturation: s brightness: b		Create a color with the given hue, saturation, and brightness.

	pink
 	blue
	red ...	Many colors have messages that return an instance of Color.
	canUnderstand: #brown	  Returns true if #brown is a defined color.
	names		An OrderedCollection of the names of the colors.
	named: #notAllThatGray put: aColor    Add a new color to the list and create an access message and a class variable for it.
	fromUser	Shows the palette of colors available at this display depth.  Click anywhere to return the color you clicked on.

	hotColdShades: thisMany	An array of thisMany colors showing temperature from blue to red to white hot.

    stdColorsForDepth: d        An Array of colors available at this depth.  For 16 bit and 32 bits, returns a ColorGenerator.  It responds to at: with a Color for that index, simulating a very big Array. 

   colorFromPixelValue: value depth: d    Returns a Color whose bit pattern (inside a Bitmap) at this depth is the number specified.  Normal users do not need to use this.

(See also comments in these classes: Form, Bitmap, BitBlt, Pattern, MaskedForm.)