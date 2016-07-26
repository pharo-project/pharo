A rectangular array of pixels, used for holding images.  All pictures, including character images are Forms.  The depth of a Form is how many bits are used to specify the color at each pixel.  The actual bits are held in a Bitmap, whose internal structure is different at each depth.  Class Color allows you to deal with colors without knowing how they are actually encoded inside a Bitmap.
	  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.
	Forms are indexed starting at 0 instead of 1; thus, the top-left pixel of a Form has coordinates 0@0.
	Forms are combined using BitBlt.  See the comment in class BitBlt.  Forms that repeat many times to fill a large destination are InfiniteForms.

	colorAt: x@y		Returns the abstract Color at this location
	displayAt: x@y		shows this form on the screen
	displayOn: aMedium at: x@y	shows this form in a Window, a Form, or other DisplayMedium
	fillColor: aColor		Set all the pixels to the color.
	edit		launch an editor to change the bits of this form.
	pixelValueAt: x@y	The encoded color.  The encoding depends on the depth.
