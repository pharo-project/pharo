"
This class represents abstract color, regardless of the depth of bitmap it will be shown in.  At the very last moment a Color is converted to a pixelValue that depends on the depth of the actual Bitmap inside the Form it will be used with.  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.  (See comment in BitBlt.)  To change the depth of the Display and set how many colors you can see, execute: (Display newDepth: 8).  (See comment in DisplayMedium)

	Color is represented as the amount of light in red, green, and blue.  White is (1.0, 1.0, 1.0) and black is (0, 0, 0).  Pure red is (1.0, 0, 0).  These colors are ""additive"".  Think of Color's instance variables as:
	r	amount of red, a Float between 0.0 and 1.0.
	g	amount of green, a Float between 0.0 and 1.0.
	b	amount of blue, a Float between 0.0 and 1.0.
(But, in fact, the three are encoded as values from 0 to 1023 and combined in a single integer, rgb.  The user does not need to know this.)

	Many colors are named.  You find a color by name by sending a message to class Color, for example (Color lightBlue).  Also, (Color red: 0.2 green: 0.6 blue: 1.0) or (Color r: 0.2 g: 0.6 b: 1.0) creates a color. (see below)
	A color is essentially immutable.  Once you set red, green, and blue, you cannot change them.  Instead, create a new Color and use it.
	
	Applications such as contour maps and bar graphs will want to display one of a set of shades based on a number.  Convert the range of this number to an integer from 1 to N.  Then call (Color green lightShades: N) to get an Array of colors from white to green.  Use the Array messages at:, atPin:, or atWrap: to pull out the correct color from the array.  atPin: gives the first (or last) color if the index is out of range.  atWrap: wraps around to the other end if the index is out of range.
		
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

   colorFromPixelValue: value depth: d    Returns a Color whose bit pattern (inside a Bitmap) at this depth is the number specified.  Normal users do not need to use this.

(See also comments in these classes: Form, Bitmap, BitBlt,.)
"
Class {
	#name : #Color,
	#superclass : #Object,
	#instVars : [
		'rgb',
		'cachedDepth',
		'cachedBitPattern',
		'alpha'
	],
	#classVars : [
		'BlueShift',
		'CachedColormaps',
		'ColorRegistry',
		'ComponentMask',
		'ComponentMax',
		'GrayToIndexMap',
		'GreenShift',
		'HalfComponentMask',
		'IndexedColors',
		'MaskingMap',
		'RedShift'
	],
	#category : #'Colors-Base'
}

{ #category : #colormaps }
Color class >> aaFontsColormapDepth [
	"Adjust balance between colored AA text quality (especially if subpixel AA is used) and space / performance.
	5 is optimal quality. Each colorMap takes 128kB of RAM, and takes several seconds to build.
	4 is a reasonable balance. Each colorMap takes 16kB of RAM and builds fast on a fast machine.
	3 is good for slow hardware or memory restrictions. Each colorMap takes 2 kb of RAM."
	^ 4
]

{ #category : #defaults }
Color class >> black [
	^ ColorRegistry at: #black
]

{ #category : #defaults }
Color class >> blue [
	^ ColorRegistry at: #blue
]

{ #category : #defaults }
Color class >> brown [
	^ ColorRegistry at: #brown
]

{ #category : #'instance creation' }
Color class >> colorFrom: parm [
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"
	"Color colorFrom: #(blue darker) >>> (Color r: 0.011 g: 0.23900000000000002 b: 0.795 alpha: 1.0) "
	"(Color colorFrom: Color blue darker)>>>(Color r: 0.011 g: 0.23900000000000002 b: 0.795 alpha: 1.0)"
	"(Color colorFrom: #blue)>>> (Color blue)"
	"(Color colorFrom: #(0.0 0.0 1.0) >>> (Color blue)"

	| aColor firstParm |
	(parm isKindOf: Color)
		ifTrue: [ ^ parm ].
	parm isSymbol
		ifTrue: [ ^ self perform: parm ].
	parm isString
		ifTrue: [ ^ self fromString: parm ].
	((parm isKindOf: SequenceableCollection) and: [ parm size > 0 ])
		ifTrue: [ firstParm := parm first.
			(firstParm isKindOf: Number)
				ifTrue: [ ^ self fromRgbTriplet: parm ].
			aColor := self colorFrom: firstParm.
			parm
				doWithIndex: [ :sym :ind | 
					ind > 1
						ifTrue: [ aColor := aColor perform: sym ] ].
			^ aColor ].
	^ parm


]

{ #category : #'instance creation' }
Color class >> colorFromPixelValue: p depth: d [ 
	"Convert a pixel value for the given display depth into a color."
	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."
	| r g b alpha |
	d = 8 ifTrue: [ ^ IndexedColors at: (p bitAnd: 255) + 1 ].
	d = 4 ifTrue: [ ^ IndexedColors at: (p bitAnd: 15) + 1 ].
	d = 2 ifTrue: [ ^ IndexedColors at: (p bitAnd: 3) + 1 ].
	d = 1 ifTrue: [ ^ IndexedColors at: (p bitAnd: 1) + 1 ].
	d = 16 | (d = 15) ifTrue: 
		[ "five bits per component"
		r := (p bitShift: -10) bitAnd: 31.
		g := (p bitShift: -5) bitAnd: 31.
		b := p bitAnd: 31.
		(r = 0 and: [ g = 0 ]) ifTrue: 
			[ b = 0 ifTrue: [ ^ self transparent ].
			b = 1 ifTrue: [ ^ self black ] ].
		^ self 
			r: r
			g: g
			b: b
			range: 31 ].
	d = 32 ifTrue: 
		[ "eight bits per component; 8 bits of alpha"
		r := (p bitShift: -16) bitAnd: 255.
		g := (p bitShift: -8) bitAnd: 255.
		b := p bitAnd: 255.
		alpha := p bitShift: -24.
		alpha = 0 ifTrue: [ ^ self transparent ].
		alpha < 255 
			ifTrue: 
				[ ^ (self 
					r: r
					g: g
					b: b
					range: 255) alpha: alpha asFloat / 255.0 ]
			ifFalse: 
				[ ^ self 
					r: r
					g: g
					b: b
					range: 255 ] ].
	d = 12 ifTrue: 
		[ "four bits per component"
		r := (p bitShift: -8) bitAnd: 15.
		g := (p bitShift: -4) bitAnd: 15.
		b := p bitAnd: 15.
		^ self 
			r: r
			g: g
			b: b
			range: 15 ].
	d = 9 ifTrue: 
		[ "three bits per component"
		r := (p bitShift: -6) bitAnd: 7.
		g := (p bitShift: -3) bitAnd: 7.
		b := p bitAnd: 7.
		^ self 
			r: r
			g: g
			b: b
			range: 7 ].
	self error: 'unknown pixel depth: ' , d printString
]

{ #category : #defaults }
Color class >> cyan [
	^ ColorRegistry at: #cyan
]

{ #category : #defaults }
Color class >> darkGray [
	^ ColorRegistry at: #darkGray
]

{ #category : #defaults }
Color class >> defaultColors [
	^{ #black. 0. 0. 0.
		#veryVeryDarkGray. 0.125. 0.125. 0.125.
		#veryDarkGray. 0.25. 0.25. 0.25.
		#darkGray. 0.375. 0.375. 0.375.
		#gray. 0.5. 0.5. 0.5.
		#lightGray. 0.625. 0.625. 0.625.
		#veryLightGray. 0.75. 0.75. 0.75.
		#veryVeryLightGray. 0.875. 0.875. 0.875.
		#white. 1.0. 1.0. 1.0.
		}
		
]

{ #category : #defaults }
Color class >> defaultColors2 [
	^{	#red. 1.0. 0. 0.
		#yellow. 1.0. 1.0. 0.
		#green. 0. 1.0. 0.
		#cyan. 0. 1.0. 1.0.
		#blue. 0. 0. 1.0.
		#magenta. 1.0. 0. 1.0.
		#brown. 0.6. 0.2. 0.
		#orange. 1.0. 0.6. 0.
		#lightRed. 1.0. 0.8. 0.8.
		
		}
	
]

{ #category : #defaults }
Color class >> defaultColors3 [ 
	^{
	#lightYellow. 1.0. 1.0. 0.8.
	#lightGreen. 0.8. 1.0. 0.6.
	#lightCyan. 0.4. 1.0. 1.0.
	#lightBlue. 0.8. 1.0. 1.0.
	#lightMagenta. 1.0. 0.8. 1.0.
	#lightBrown. 1.0. 0.6. 0.2.
	#lightOrange. 1.0. 0.8. 0.4.
	#pink. 1.0. 0.752899. 0.796118.
	#purple. 0.4. 0.0. 0.6.
	#tan. 0.8. 0.8. 0.5.
	#veryPaleRed. 1.0. 0.948. 0.948.
	#paleYellow. 1.0. 1.0. 0.85.
	#paleTan. 0.921. 0.878. 0.78.
	}
]

{ #category : #defaults }
Color class >> defaultColors4 [
	^{
	#paleRed. 1.0. 0.901. 0.901.
	#palePeach. 1.0. 0.929. 0.835.
	#paleOrange. 0.991. 0.929. 0.843.
	#paleMagenta. 1.0. 0.901. 1.0.
	#paleGreen. 0.874. 1.0. 0.835.
	#paleBuff. 0.995. 0.979. 0.921.
	#paleBlue. 0.87. 0.976. 0.995.
		}
]

{ #category : #'instance creation' }
Color class >> fromArray: colorDef [
	colorDef size = 3
			ifTrue: [^self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3)].
	colorDef size = 0
			ifTrue: [^self transparent].
	colorDef size = 4
			ifTrue: [^(self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3)) alpha: (colorDef at: 4)].
	self error: 'Undefined color definition'
]

{ #category : #'instance creation' }
Color class >> fromHexString: aColorHex [
	"Returns a color instance from HEX (6 element elements)."

	"(Color fromHexString: 'FFFFFF')>>> (Color white) "

	| green red blue alpha hexString |
	hexString := aColorHex withoutPrefix: '#'.
	red := (Integer readFrom: (hexString first: 2) base: 16) / 255.
	green := (Integer readFrom: (hexString copyFrom: 3 to: 4) base: 16) / 255.
	blue := (Integer readFrom: (hexString copyFrom: 5 to: 6) base: 16) / 255.
	
	alpha := hexString size = 8 
		ifFalse: [ 1.0 ]
		ifTrue: [ (Integer readFrom: (hexString copyFrom: 7 to: 8) base: 16) / 255 ].
	
	^ self r: red g: green b: blue alpha: alpha
]

{ #category : #'instance creation' }
Color class >> fromRgbTriplet: list [
	"Return a color from its RGB components (0 - 1.0 floats)"
	"(Color colorFrom: #(1.0 1.0 1.0)) >>> (Color white)"

	^ self r: list first g: list second b: list last
]

{ #category : #'instance creation' }
Color class >> fromString: aString [
	"Return a color for HTML color spec: #FFCCAA or white/black passed as string."

	"(Color fromString: '#FFCCAA')>>> (Color r: 1.0 g: 0.8 b: 0.667 alpha: 1.0) "
	"(Color fromString: 'orange') >>> Color orange"

	| aColorHex |
	aString isEmptyOrNil
		ifTrue: [ ^ self white ].
	aColorHex := aString first = $#
		ifTrue: [ aString allButFirst ]
		ifFalse: [ aString ].	"Try to match aColorHex with known named colors, case insensitive."
	^ self registeredColorNames
		detect: [ :each | each sameAs: aColorHex ]
		ifFound: [ :namedColor | self named: namedColor ]
		ifNone: [ (aColorHex size = 6 and: [ aColorHex allSatisfy: [ :character | '0123456789ABCDEFabcdef' includes: character ] ])
				ifTrue: [ self fromHexString: aColorHex ]
				ifFalse: [ self white ] ]
]

{ #category : #defaults }
Color class >> gray [
	^ ColorRegistry at: #gray
]

{ #category : #'instance creation' }
Color class >> gray: brightness [
	"Return a gray shade with the given brightness in the range [0.0..1.0]."

	^ self r: brightness g: brightness b: brightness

]

{ #category : #defaults }
Color class >> green [
	^ ColorRegistry at: #green
]

{ #category : #'instance creation' }
Color class >> h: hue s: saturation l: lightness [
	"Create a color with the given hue, saturation, and lightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and lightness are numbers in [0.0..1.0] where larger values are more saturated or lighter colors. The difference with brightness in the HSV coordinates is that colors go from black at lightness 0, through vivid hues at lightness 0.5, to white at lightness 1. For example, (Color h: 0 s: 1 l: 0.5) is pure red."
	" (Color h: 0 s: 1 l: 0.5) >>> (Color r: 1 g: 0 b:0)"

	^ self
		h: hue
		s: saturation
		l: lightness
		alpha: 1.0
]

{ #category : #'instance creation' }
Color class >> h: hue s: saturation l: lightness alpha: alpha [
	"Create a color with the given hue, saturation, lightness, and alpha. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and lightness are numbers in [0.0..1.0] where larger values are more saturated or lighter colors. The difference with brightness in the HSV coordinates is that colors go from black at lightness 0, through vivid hues at lightness 0.5, to white at lightness 1. For example, (Color h: 0 s: 1 l: 0.5) is pure red."
	^ self basicNew
		initializeHue: hue
			saturation: saturation
			lightness: lightness
			alpha: alpha;
		yourself
]

{ #category : #'instance creation' }
Color class >> h: hue s: saturation v: brightness [
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."
	"(Color h: 0 s: 1 v: 1) >>> (Color r: 1 g: 0 b:0)"
	^ self h: hue s: saturation v: brightness alpha: 1.0
]

{ #category : #'instance creation' }
Color class >> h: hue s: saturation v: brightness alpha: alpha [
	"Create a color with the given hue, saturation, brightness, and alpha. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1 alpha: 1) is pure red."
	"(Color h: 0 s: 1 v: 1 alpha: 1) >>> Color red "
	"(Color h: 0 s: 1 v: 1 alpha: 0.5) >>> (Color r: 1.0 g: 0.0 b: 0.0 alpha: 0.498) "
	^ self basicNew
		initializeHue: hue saturation: saturation brightness: brightness alpha: alpha ;
		yourself 
	
]

{ #category : #other }
Color class >> hex: aFloat [
	"Return an hexadecimal two-digits string between 00 and FF
	for a float between 0.0 and 1.0"
	" Color hex: 0.2>> '33' "
	| str |
	str := (aFloat * 255) asInteger printStringHex asLowercase.
	str size = 1
		ifTrue: [ ^ '0' , str ]
		ifFalse: [ ^ str ]
]

{ #category : #other }
Color class >> indexedColors [

	^ IndexedColors
]

{ #category : #'initialize-release' }
Color class >> initialize [
	"self initialize
	
	Details: Externally, the red, green, and blue components of color
	are floats in the range [0.0..1.0]. Internally, they are represented
	as integers in the range [0..ComponentMask] packing into a
	small integer to save space and to allow fast hashing and
	equality testing.

	For a general description of color representations for computer
	graphics, including the relationship between the RGB and HSV
	color models used here, see Chapter 17 of Foley and van Dam,
	Fundamentals of Interactive Computer Graphics, Addison-Wesley,
	1982."
	ComponentMask := 1023.
	HalfComponentMask := 512.	"used to round up in integer calculations"
	ComponentMax := 1023.0.	"a Float used to normalize components"
	RedShift := 20.
	GreenShift := 10.
	BlueShift := 0.
	self initializeIndexedColors.
	self initializeColorRegistry.
	self initializeGrayToIndexMap.
	
	SessionManager default registerGuiClassNamed: self name
]

{ #category : #'initialize-release' }
Color class >> initializeColorRegistry [ 
	| values|
	ColorRegistry := IdentityDictionary new.
	values := self defaultColors, self defaultColors2, self defaultColors3, self defaultColors4.
	1 to: values size by: #(name r g b) size do:[:index|
		|  colorName red green blue color |
		colorName := values at: index.
		red := values at: index + 1.
		green := values at: index + 2.
		blue := values at: index +3.
		color :=  self  r: red g: green b: blue .
		self registerColor: color named: colorName ].
	self registerColor: (self r: 0 g: 0 b: 0 alpha: 0.0) named: #transparent. 
]

{ #category : #'initialize-release' }
Color class >> initializeGrayToIndexMap [
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"Color initializeGrayToIndexMap"
	"record the level and index of each gray in the 8-bit color table"
	| grayLevels grayIndices c distToClosest dist indexOfClosest |
	grayLevels := OrderedCollection new.
	grayIndices := OrderedCollection new.
	"Note: skip the first entry, which is reserved for transparent"
	2 
		to: IndexedColors size
		do: 
			[ :i | 
			c := IndexedColors at: i.
			c saturation = 0.0 ifTrue: 
				[ "c is a gray"
				grayLevels add: c privateBlue >> 2.	"top 8 bits; R, G, and B are the same"
				grayIndices add: i - 1 ] ].	"pixel values are zero-based"
	grayLevels := grayLevels asArray.
	grayIndices := grayIndices asArray.

	"for each gray level in [0..255], select the closest match"
	GrayToIndexMap := ByteArray new: 256.
	0 
		to: 255
		do: 
			[ :level | 
			distToClosest := 10000.	"greater than distance to any real gray"
			1 
				to: grayLevels size
				do: 
					[ :i | 
					dist := (level - (grayLevels at: i)) abs.
					dist < distToClosest ifTrue: 
						[ distToClosest := dist.
						indexOfClosest := grayIndices at: i ] ].
			GrayToIndexMap 
				at: level + 1
				put: indexOfClosest ]
]

{ #category : #'initialize-release' }
Color class >> initializeIndexedColors [
	"Build an array of colors corresponding to the fixed colormap used
	 for display depths of 1, 2, 4, or 8 bits."
	"Color initializeIndexedColors"
	| a index grayVal |
	a := Array new: 256.

	"1-bit colors (monochrome)"
	a 
		at: 1
		put: (self 
				r: 1.0
				g: 1.0
				b: 1.0).	"white or transparent"
	a 
		at: 2
		put: (self 
				r: 0.0
				g: 0.0
				b: 0.0).	"black"

	"additional colors for 2-bit color"
	a 
		at: 3
		put: (self 
				r: 1.0
				g: 1.0
				b: 1.0).	"opaque white"
	a 
		at: 4
		put: (self 
				r: 0.5
				g: 0.5
				b: 0.5).	"1/2 gray"

	"additional colors for 4-bit color"
	a 
		at: 5
		put: (self 
				r: 1.0
				g: 0.0
				b: 0.0).	"red"
	a 
		at: 6
		put: (self 
				r: 0.0
				g: 1.0
				b: 0.0).	"green"
	a 
		at: 7
		put: (self 
				r: 0.0
				g: 0.0
				b: 1.0).	"blue"
	a 
		at: 8
		put: (self 
				r: 0.0
				g: 1.0
				b: 1.0).	"cyan"
	a 
		at: 9
		put: (self 
				r: 1.0
				g: 1.0
				b: 0.0).	"yellow"
	a 
		at: 10
		put: (self 
				r: 1.0
				g: 0.0
				b: 1.0).	"magenta"
	a 
		at: 11
		put: (self 
				r: 0.125
				g: 0.125
				b: 0.125).	"1/8 gray"
	a 
		at: 12
		put: (self 
				r: 0.25
				g: 0.25
				b: 0.25).	"2/8 gray"
	a 
		at: 13
		put: (self 
				r: 0.375
				g: 0.375
				b: 0.375).	"3/8 gray"
	a 
		at: 14
		put: (self 
				r: 0.625
				g: 0.625
				b: 0.625).	"5/8 gray"
	a 
		at: 15
		put: (self 
				r: 0.75
				g: 0.75
				b: 0.75).	"6/8 gray"
	a 
		at: 16
		put: (self 
				r: 0.875
				g: 0.875
				b: 0.875).	"7/8 gray"

	"additional colors for 8-bit color"
	"24 more shades of gray (1/32 increments but not repeating 1/8 increments)"
	index := 17.
	1 
		to: 31
		do: 
			[ :v | 
			v \\ 4 = 0 ifFalse: 
				[ grayVal := v / 32.0.
				a 
					at: index
					put: (Color 
							r: grayVal
							g: grayVal
							b: grayVal).
				index := index + 1 ] ].

	"The remainder of color table defines a color cube with six steps
	 for each primary color. Note that the corners of this cube repeat
	 previous colors, but this simplifies the mapping between RGB colors
	 and color map indices. This color cube spans indices 40 through 255
	 (indices 41-256 in this 1-based array)."
	0 
		to: 5
		do: 
			[ :r | 
			0 
				to: 5
				do: 
					[ :g | 
					0 
						to: 5
						do: 
							[ :b | 
							index := 41 + (36 * r + (6 * b) + g).
							index > 256 ifTrue: [ self error: 'index out of range in color table compuation' ].
							a 
								at: index
								put: (self 
										r: r
										g: g
										b: b
										range: 5) ] ] ].
	IndexedColors := a
]

{ #category : #defaults }
Color class >> lightBlue [
	^ ColorRegistry at: #lightBlue
]

{ #category : #defaults }
Color class >> lightBrown [
	^ ColorRegistry at: #lightBrown
]

{ #category : #defaults }
Color class >> lightCyan [
	^  ColorRegistry at: #lightCyan
]

{ #category : #defaults }
Color class >> lightGray [
	^  ColorRegistry at: #lightGray
]

{ #category : #defaults }
Color class >> lightGreen [
	^ ColorRegistry at: #lightGreen
]

{ #category : #defaults }
Color class >> lightMagenta [
	^  ColorRegistry at: #lightMagenta
]

{ #category : #defaults }
Color class >> lightOrange [
	^ ColorRegistry at: #lightOrange
]

{ #category : #defaults }
Color class >> lightRed [
	^  ColorRegistry at: #lightRed
]

{ #category : #defaults }
Color class >> lightYellow [
	^  ColorRegistry at: #lightYellow
]

{ #category : #defaults }
Color class >> magenta [
	^ ColorRegistry at: #magenta
]

{ #category : #accessing }
Color class >> named: aColorName [
	"Return the color associated with the name aColorName and nil otherwise. Note that only a subsets of colors have a name."

	^ ColorRegistry at: aColorName asSymbol ifAbsent: nil
]

{ #category : #'instance creation' }
Color class >> new [

	^ self r: 0.0 g: 0.0 b: 0.0
]

{ #category : #defaults }
Color class >> orange [
	^ ColorRegistry at: #orange
]

{ #category : #defaults }
Color class >> paleBlue [
	^  ColorRegistry at: #paleBlue 
]

{ #category : #defaults }
Color class >> paleBuff [
	^  ColorRegistry at: #paleBuff
]

{ #category : #defaults }
Color class >> paleGreen [
	^  ColorRegistry at: #paleGreen
]

{ #category : #defaults }
Color class >> paleMagenta [
	^  ColorRegistry at: #paleMagenta
]

{ #category : #defaults }
Color class >> paleOrange [
	^  ColorRegistry at: #paleOrange
]

{ #category : #defaults }
Color class >> palePeach [
	^  ColorRegistry at: #palePeach 
]

{ #category : #defaults }
Color class >> paleRed [
	^  ColorRegistry at: #paleRed
]

{ #category : #defaults }
Color class >> paleTan [
	^  ColorRegistry at: #paleTan
]

{ #category : #defaults }
Color class >> paleYellow [
	^  ColorRegistry at: #paleYellow.
]

{ #category : #defaults }
Color class >> pink [
 	^  ColorRegistry at: #pink.
]

{ #category : #defaults }
Color class >> purple [
 	^  ColorRegistry at: #purple
]

{ #category : #'instance creation' }
Color class >> r: r g: g b: b [
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."
	"(Color r: 1 g: 1 b: 1) >>> Color white"
	
	^ self r: r g: g b: b alpha: 1.0
]

{ #category : #'instance creation' }
Color class >> r: r g: g b: b alpha: alpha [
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."
	^ self basicNew
		initializeRed: r green: g blue: b alpha: alpha ;
		yourself.
]

{ #category : #'instance creation' }
Color class >> r: r g: g b: b range: range [
	"Return a color with the given r, g, and b components specified as integers in the range [0..r]. This avoids the floating point arithmetic in the red:green:blue: message and is thus a bit faster for certain applications (such as computing a sequence of colors for a palette)."

	"(Color r: 31 g: 0 b: 0 range: 31) >>> (Color r: 1.0 g: 0 b: 0)"

	^ self basicNew
		initializeRed: r
			green: g
			blue: b
			range: range;
		yourself
]

{ #category : #defaults }
Color class >> red [
	^  ColorRegistry at: #red
]

{ #category : #'initialize-release' }
Color class >> registerColor: aColor named: aName [
	ColorRegistry at: aName put: aColor.
	
]

{ #category : #accessing }
Color class >> registeredColorNames [
	"Returns all the available names of named colors."
	
	^ ColorRegistry keys collect: #asString
]

{ #category : #accessing }
Color class >> registeredNameOf: aColor [
	"Return the name of a color if it has a name, else #unnamed"
	| colorName |
	colorName := #unnamed.
	ColorRegistry
		keysAndValuesDo: [ :key :value | 
			value = aColor
				ifTrue: [ colorName := key ] ].
	^ colorName
]

{ #category : #'system startup' }
Color class >> shutDown [
	"Color shutDown"
	CachedColormaps := nil.	"Maps to translate between color depths"
	MaskingMap := nil	"Maps all colors except transparent to black for creating a mask"
]

{ #category : #defaults }
Color class >> tan [
	^  ColorRegistry at: #tan
]

{ #category : #defaults }
Color class >> transparent [
  ^   ColorRegistry at: #transparent 
]

{ #category : #accessing }
Color class >> unregisterColorNamed: aName [
	"Remove the color named aName from the list of named colors."
	ColorRegistry removeKey: aName ifAbsent: nil
]

{ #category : #defaults }
Color class >> veryDarkGray [
	^ ColorRegistry at: #veryDarkGray
]

{ #category : #defaults }
Color class >> veryLightGray [
	^ ColorRegistry at: #veryLightGray
]

{ #category : #defaults }
Color class >> veryPaleRed [
	^  ColorRegistry at: #veryPaleRed
]

{ #category : #defaults }
Color class >> veryVeryDarkGray [
	^ ColorRegistry at: #veryVeryDarkGray 
]

{ #category : #defaults }
Color class >> veryVeryLightGray [
	^  ColorRegistry at: #veryVeryLightGray
]

{ #category : #examples }
Color class >> wheel: thisMany [
	"Return a collection of thisMany colors evenly spaced around the color wheel."
	"(Color wheel: 12) inspect"

	^ self wheel: thisMany saturation: 0.9 brightness: 0.7

]

{ #category : #examples }
Color class >> wheel: thisMany saturation: s brightness: v [
	"Return a collection of thisMany colors evenly spaced around the color wheel, all of the given saturation and brightness."
	"(Color wheel: 12 saturation: 0.4 brightness: 1.0) inspect"
	"(Color wheel: 12 saturation: 0.8 brightness: 0.5) inspect"

	^ (self h: 0.0 s: s v: v) wheel: thisMany

]

{ #category : #defaults }
Color class >> white [
	^  ColorRegistry at: #white
]

{ #category : #defaults }
Color class >> yellow [
	 ^  ColorRegistry at: #yellow
]

{ #category : #transformations }
Color >> * aNumberOrArray [
	"Answer this color with its RGB multiplied by the given number, or
	 multiply this color's RGB values by the corresponding entries in the
	given array."
	"(self brown * 2) display"
	"(self brown * #(1 0 1)) display"
	| multipliers |
	multipliers := aNumberOrArray isCollection
		ifTrue: [aNumberOrArray]
		ifFalse:
			[Array
				with: aNumberOrArray
				with: aNumberOrArray
				with: aNumberOrArray].

	^ self class basicNew
		setPrivateRed: (self privateRed * multipliers first) asInteger
		green: (self privateGreen * multipliers second) asInteger
		blue: (self privateBlue * multipliers third) asInteger;
		setAlpha: self alpha
]

{ #category : #transformations }
Color >> + aColor [
	"Answer this color mixed with the given color in an additive color space.  "

	^ self class basicNew
		setPrivateRed: self privateRed + aColor privateRed
		green: self privateGreen + aColor privateGreen
		blue: self privateBlue + aColor  privateBlue;
		setAlpha: (self alpha + aColor alpha min: 1.0)

]

{ #category : #transformations }
Color >> - aColor [
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"(Color white - Color red) display"

	^ self class basicNew
		setPrivateRed: self privateRed - aColor privateRed
		green: self privateGreen - aColor privateGreen
		blue: self privateBlue - aColor  privateBlue;
		setAlpha: ((self = aColor) ifTrue: [ 0 ] ifFalse: [ self alpha ])

]

{ #category : #transformations }
Color >> / aNumber [
	"Answer this color with its RGB divided by the given number. "
	"(Color red / 2) display"

	^ self class basicNew
		setPrivateRed: (self privateRed / aNumber) asInteger
		green: (self privateGreen / aNumber) asInteger
		blue: (self privateBlue / aNumber) asInteger;
		setAlpha: self alpha

]

{ #category : #comparing }
Color >> = aColor [
	"Return true if the receiver equals the given color. This method handles translucent colors, too."

	aColor isColor ifFalse: [^ false].
	^ aColor privateRGB = rgb and:
		[aColor privateAlpha = self privateAlpha]

]

{ #category : #transformations }
Color >> adjustBrightness: brightness [
	"Adjust the relative brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ self class
		h: self hue
		s: self saturation
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha
]

{ #category : #transformations }
Color >> adjustSaturation: saturation brightness: brightness [
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ self class
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha
]

{ #category : #accessing }
Color >> alpha [
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ alpha asFloat / 255.0
]

{ #category : #transformations }
Color >> alpha: aFloat [ 
	"Answer a new Color with the given amount of opacity ('alpha')."

	^ self class r: self red g: self green b: self blue alpha: aFloat 
]

{ #category : #transformations }
Color >> alphaMixed: proportion with: aColor [ 
	"Answer this color mixed with the given color. The proportion, a number 
	between 0.0 and 1.0, determines what what fraction of the receiver to  
	use in the mix. For example, 0.9 would yield a color close to the  
	receiver. This method uses RGB interpolation; HSV interpolation can lead 
	to surprises.  Mixes the alphas (for transparency) also."
	| frac1 frac2 |
	frac1 := proportion asFloat 
		min: 1.0
		max: 0.0.
	frac2 := 1.0 - frac1.
	^ self class 
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
		alpha: self alpha * frac1 + (aColor alpha * frac2)
]

{ #category : #conversions }
Color >> asColor [
	"Convert the receiver into a color"
	^self
]

{ #category : #conversions }
Color >> asColorref [
	"Convert the receiver into a colorref, i.e, a number embedding r,g,b on 24 bits"
	"(Color red asColorref) >>> 255"
	"(Color white asColorref) >>>  16777215"
	
	^(self red * 255) asInteger + ((self green * 255) asInteger << 8) + ((self blue * 255) asInteger << 16)
]

{ #category : #conversions }
Color >> asHexString [
	"Return a string representing the receiver in HEX (i.e., 6 elements in 16 basis)"
	"(Color red asHexString) >>> 'FF0000'"
	| s |
	s := '000000' copy.
	s at: 1 put: (Character digitValue: ((rgb bitShift: -6 - RedShift) bitAnd: 15)).
	s at: 2 put: (Character digitValue: ((rgb bitShift: -2 - RedShift) bitAnd: 15)).
	s at: 3 put: (Character digitValue: ((rgb bitShift: -6 - GreenShift) bitAnd: 15)).
	s at: 4 put: (Character digitValue: ((rgb bitShift: -2 - GreenShift) bitAnd: 15)).
	s at: 5 put: (Character digitValue: ((rgb bitShift: -6 - BlueShift) bitAnd: 15)).
	s at: 6 put: (Character digitValue: ((rgb bitShift: -2 - BlueShift) bitAnd: 15)).
	^ s
]

{ #category : #conversions }
Color >> asNontranslucentColor [
	"Set the transparency of the receiver to opaque."
	
	^ self alpha: 1.0.
]

{ #category : #transformations }
Color >> atLeastAsLuminentAs: aFloat [ 
	| revisedColor |
	revisedColor := self.
	[ revisedColor luminance < aFloat ] whileTrue: [ revisedColor := revisedColor slightlyLighter ].
	^ revisedColor
]

{ #category : #transformations }
Color >> atMostAsLuminentAs: aFloat [ 
	| revisedColor |
	revisedColor := self.
	[ revisedColor luminance > aFloat ] whileTrue: [ revisedColor := revisedColor slightlyDarker ].
	^ revisedColor
]

{ #category : #private }
Color >> attemptToMutateError [
	"A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it."

	self error: 'Color objects are immutable once created'

]

{ #category : #conversions }
Color >> basicPixelValueForDepth: d [ 
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8, black maps to the darkest possible blue."
	| rgbBlack val |
	d = 8 ifTrue: [ ^ self closestPixelValue8 ].	"common case"
	d < 8 ifTrue: 
		[ d = 4 ifTrue: [ ^ self closestPixelValue4 ].
		d = 2 ifTrue: [ ^ self closestPixelValue2 ].
		d = 1 ifTrue: [ ^ self closestPixelValue1 ] ].
	rgbBlack := 1.	"closest black that is not transparent in RGB"
	d = 16 ifTrue: 
		[ "five bits per component; top bits ignored"
		val := (((rgb bitShift: -15) bitAnd: 31744) bitOr: ((rgb bitShift: -10) bitAnd: 992)) bitOr: ((rgb bitShift: -5) bitAnd: 31).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	d = 32 ifTrue: 
		[ "eight bits per component; top 8 bits set to all ones (opaque alpha)"
		val := LargePositiveInteger new: 4.
		val 
			at: 3
			put: ((rgb bitShift: -22) bitAnd: 255).
		val 
			at: 2
			put: ((rgb bitShift: -12) bitAnd: 255).
		val 
			at: 1
			put: ((rgb bitShift: -2) bitAnd: 255).
		val 
			at: 4
			put: 255.	"opaque alpha"
		^ val ].
	d = 12 ifTrue: 
		[ "for indexing a color map with 4 bits per color component"
		val := (((rgb bitShift: -18) bitAnd: 3840) bitOr: ((rgb bitShift: -12) bitAnd: 240)) bitOr: ((rgb bitShift: -6) bitAnd: 15).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	d = 9 ifTrue: 
		[ "for indexing a color map with 3 bits per color component"
		val := (((rgb bitShift: -21) bitAnd: 448) bitOr: ((rgb bitShift: -14) bitAnd: 56)) bitOr: ((rgb bitShift: -7) bitAnd: 7).
		^ val = 0 
			ifTrue: [ rgbBlack ]
			ifFalse: [ val ] ].
	self error: 'unknown pixel depth: ' , d printString
]

{ #category : #'accessing - higher level' }
Color >> beOpaque [
	"Set the transparency of the receiver to opaque, i.e. alpha to 1.0."
	
	^ self alpha: 1.0
]

{ #category : #transformations }
Color >> blacker [
	
	^ self alphaMixed: 0.8333 with: self class black

]

{ #category : #accessing }
Color >> blue [
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self privateBlue asFloat / ComponentMax
]

{ #category : #accessing }
Color >> brightness [
	"Return the HSV brightness of this color, a float in the range [0.0..1.0]."

	^ self privateMaxComponent asFloat / ComponentMax
]

{ #category : #accessing }
Color >> chroma [
	"Return the chroma of this color, a float in the range [0.0..1.0].
	Some combinations of chroma and brightness (or lightness) are meaningless in RGB; therefore, HSL and HSV each define a specific saturation by scaling chroma to the full [0..1] range."

	^ (self privateMaxComponent - self privateMinComponent) asFloat / ComponentMax
]

{ #category : #conversions }
Color >> closestPixelValue1 [
	"Return the nearest approximation to this color for a monochrome Form."

	"fast special cases"
	rgb = 0 ifTrue: [^ 1].  "black"
	rgb = 16r3FFFFFFF ifTrue: [^ 0].  "white"

	self luminance > 0.5
		ifTrue: [^ 0]  "white"
		ifFalse: [^ 1].  "black"

]

{ #category : #conversions }
Color >> closestPixelValue2 [
	"Return the nearest approximation to this color for a 2-bit deep Form."
	"fast special cases"
	| lum |
	rgb = 0 ifTrue: [ ^ 1 ].	"black"
	rgb = 1073741823 ifTrue: [ ^ 2 ].	"opaque white"
	lum := self luminance.
	lum < 0.2 ifTrue: [ ^ 1 ].	"black"
	lum > 0.6 ifTrue: [ ^ 2 ].	"opaque white"
	^ 3	"50% gray"
]

{ #category : #conversions }
Color >> closestPixelValue4 [
	"Return the nearest approximation to this color for a 4-bit deep Form."
	"fast special cases"
	| bIndex |
	rgb = 0 ifTrue: [ ^ 1 ].	"black"
	rgb = 1073741823 ifTrue: [ ^ 2 ].	"opaque white"
	rgb = self class red privateRGB ifTrue: [ ^ 4 ].
	rgb = self class green privateRGB ifTrue: [ ^ 5 ].
	rgb = self class blue privateRGB ifTrue: [ ^ 6 ].
	rgb = self class cyan privateRGB ifTrue: [ ^ 7 ].
	rgb = self class yellow privateRGB ifTrue: [ ^ 8 ].
	rgb = self class magenta privateRGB ifTrue: [ ^ 9 ].
	bIndex := (self luminance * 8.0) rounded.	"bIndex in [0..8]"
	^ #(1 10 11 12 3 13 14 15 2 ) at: bIndex + 1	"black"	"1/8 gray"	"2/8 gray"	"3/8 gray"	"4/8 gray"	"5/8 gray"	"6/8 gray"	"7/8 gray"	"opaque white"
]

{ #category : #conversions }
Color >> closestPixelValue8 [
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	rgb = 0 ifTrue: [^ 1].  "black"
	rgb = 16r3FFFFFFF ifTrue: [^ 255].  "white"

	self saturation < 0.2 ifTrue: [
		^ GrayToIndexMap at: (self privateGreen >> 2) + 1.  "nearest gray"
	] ifFalse: [
		"compute nearest entry in the color cube"
		^ 40 +
		  ((((self privateRed * 5) + HalfComponentMask) // ComponentMask) * 36) +
		  ((((self privateBlue * 5) + HalfComponentMask) // ComponentMask) * 6) +
		  (((self privateGreen * 5) + HalfComponentMask) // ComponentMask)].

]

{ #category : #other }
Color >> colorForInsets [
	^ self
]

{ #category : #conversions }
Color >> contrastingBlackAndWhiteColor [
	"Answer black or white depending on the luminance."

	self isTransparent ifTrue: [ ^ self class black].
	^self luminance > 0.5
		ifTrue: [self class black]
		ifFalse: [self class white]
]

{ #category : #conversions }
Color >> contrastingColorAdjustment [
	"Answer darker or lighter color depending on the luminance."

	self isTransparent ifTrue: [ ^ self ].
	
	^ self luminance > 0.5
		ifTrue: [ self darker ] 
		ifFalse:  [ self lighter ]
]

{ #category : #transformations }
Color >> dansDarker [
	"Return a darker shade of the same color.
	An attempt to do better than the current darker method.
	(now obsolete, since darker has been changed to do this. -dew)"
	^ self class 
		h: self hue s: self saturation v: (self brightness - 0.16 max: 0.0)
]

{ #category : #wheels }
Color >> darkShades: thisMany [
	"Return an array of thisMany colors from black to the receiver.  Array is of length num. Very useful for displaying color based on a variable in your program.  "
	"(Color red darkShades: 12) inspect"

	^ self class black mix: self shades: thisMany

]

{ #category : #transformations }
Color >> darker [
	"Answer a darker shade of this color."

	^ self adjustBrightness: -0.08
]

{ #category : #equality }
Color >> diff: theOther [
	"Returns a number between 0.0 and 1.0"
	"(Color red diff: Color red) >>> 0.0"
	"(Color white diff: Color black ) >>> 1.0"
	
	^ ((self privateRed - theOther privateRed) abs
		+ (self privateGreen - theOther privateGreen) abs
		+ (self privateBlue - theOther privateBlue) abs)
		/ 3.0 / ComponentMax
]

{ #category : #conversions }
Color >> dominantColor [
	^ self
]

{ #category : #transformations }
Color >> duller [
	"Answer a darker, desaturated color.  If the original color isn't very saturated, desaturate it by less (otherwise will just end up with grey)."
	
	| sat adjust |
	(sat := self saturation) > 0.3
		ifTrue: [adjust := -0.1]
		ifFalse: [adjust := 0.1 - sat max: 0.0].
	^ self adjustSaturation: adjust brightness: -0.1

	"^ self adjustSaturation: -0.03 brightness: -0.2"
]

{ #category : #private }
Color >> flushCache [
	"Flush my cached bit pattern."
	cachedDepth := nil.
	cachedBitPattern := nil
]

{ #category : #accessing }
Color >> green [
	"Return the green component of this color, a float in the range [0.0..1.0]."
	"Color red green >>> 0.0"
	"(Color r: 0.5 g: 0.5 b: 1) green >>>  0.5004887585532747"
	
	^ self privateGreen asFloat / ComponentMax
]

{ #category : #comparing }
Color >> hash [

	^ rgb bitXor: alpha 
]

{ #category : #accessing }
Color >> hslSaturation [
	"Return the HSL saturation of this color, a value between 0.0 and 1.0."
	"Color red hslSaturation >>> 1.0"
	"(Color r: 0.5 g: 0.5 b: 1) hslSaturation >>> 1.0"
	| chroma |
	chroma := self chroma.
	"Chroma is zero for black and white, which are the two cases of division by zero below."
	chroma isZero ifTrue: [ ^ chroma ].
	
	^ chroma / (1.0 - (self lightness * 2.0 - 1.0) abs)
]

{ #category : #accessing }
Color >> hsvSaturation [
	"Return the HSV saturation of this color, a value between 0.0 and 1.0."
	"Color red hsvSaturation >>> 1.0"
	"(Color r: 0.5 g: 0.5 b: 1) hsvSaturation >>> 0.4995112414467253"
	
	| max |
	max := self privateMaxComponent.
	max isZero
		ifTrue: [ ^ 0.0 ]
		ifFalse: [ ^ (max - self privateMinComponent) asFloat / max asFloat ]
]

{ #category : #accessing }
Color >> hue [
	"Return the hue of this color, an angle in the range [0.0..360.0]."
	"Color red hue >>> 0.0"
	"(Color r: 0.5 g: 0.5 b: 1) hue >>> 240.0"
	
	| r g b max min span h |
	r := self privateRed.
	g := self privateGreen.
	b := self privateBlue.
	max := (r max: g) max: b.
	min := (r min: g) min: b.
	span := (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].
	r = max 
		ifTrue: [ h := (g - b) asFloat / span * 60.0 ]
		ifFalse: 
			[ g = max 
				ifTrue: [ h := 120.0 + ((b - r) asFloat / span * 60.0) ]
				ifFalse: [ h := 240.0 + ((r - g) asFloat / span * 60.0) ] ].
	h < 0.0 ifTrue: [ h := 360.0 + h ].
	^ h
]

{ #category : #conversions }
Color >> indexInMap: aColorMap [
	"Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.  "

	aColorMap size = 2 ifTrue: [^ (self pixelValueForDepth: 1) + 1].
	aColorMap size = 4 ifTrue: [^ (self pixelValueForDepth: 2) + 1].
	aColorMap size = 16 ifTrue: [^ (self pixelValueForDepth: 4) + 1].
	aColorMap size = 256 ifTrue: [^ (self pixelValueForDepth: 8) + 1].
	aColorMap size = 512 ifTrue: [^ (self pixelValueForDepth: 9) + 1].
	aColorMap size = 4096 ifTrue: [^ (self pixelValueForDepth: 12) + 1].
	aColorMap size = 32768 ifTrue: [^ (self pixelValueForDepth: 16) + 1].
	self error: 'unknown pixel depth'.

]

{ #category : #initialization }
Color >> initializeHue: hue saturation: saturation brightness: brightness alpha: anAlpha [
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t |
	rgb == nil
		ifFalse: [ self attemptToMutateError ].
	s := saturation asFloat min: 1.0 max: 0.0.
	v := brightness asFloat min: 1.0 max: 0.0.
	
	"zero saturation yields gray with the given brightness"
	s = 0.0 ifTrue: [
		^ self
			initializeRed: v
			green: v
			blue: v
			alpha: anAlpha ].
	
	hf := hue asFloat \\ 360.0 / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"
		
	p := (1.0 - s) * v.
	q := (1.0 - (s * f)) * v.
	t := (1.0 - (s * (1.0 - f))) * v.
	
	0 = i
		ifTrue: [ 
			^ self
				initializeRed: v
				green: t
				blue: p
				alpha: anAlpha ].
	1 = i
		ifTrue: [ 
			^ self
				initializeRed: q
				green: v
				blue: p
				alpha: anAlpha ].
	2 = i
		ifTrue: [ 
			^ self
				initializeRed: p
				green: v
				blue: t
				alpha: anAlpha ].
	3 = i
		ifTrue: [ 
			^ self
				initializeRed: p
				green: q
				blue: v
				alpha: anAlpha ].
	4 = i
		ifTrue: [ 
			^ self
				initializeRed: t
				green: p
				blue: v
				alpha: anAlpha ].
	5 = i
		ifTrue: [ 
			^ self
				initializeRed: v
				green: p
				blue: q
				alpha: anAlpha ].
	self error: 'implementation error'
]

{ #category : #initialization }
Color >> initializeHue: hue saturation: saturation lightness: lightness alpha: anAlpha [
	"Initialize this color to the given hue, saturation, and lightness. See the comment in the instance creation method for details."

	| s l hf i f lo hi up dn |
	rgb == nil
		ifFalse: [ self attemptToMutateError ].
	s := saturation asFloat min: 1.0 max: 0.0.
	l := lightness asFloat min: 1.0 max: 0.0.

	"zero saturation yields gray with the given lightness"
	s = 0.0
		ifTrue: [ ^ self initializeRed: l green: l blue: l alpha: anAlpha ].

	hf := hue asFloat \\ 360.0 / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"

	"Color components follow a step function between levels hi & lo with sloped transitions up & dn. Each component is phased according to hue."
	hi := l < 0.5 ifTrue: [(1.0 + s) * l] ifFalse: [l + s - (l * s)].
	lo := 2.0 * l - hi.
	up := (hi - lo) * f + lo.
	dn := (lo - hi) * f + hi.

	0 = i ifTrue: [ ^ self initializeRed: hi green: up blue: lo alpha: anAlpha ].
	1 = i ifTrue: [ ^ self initializeRed: dn green: hi blue: lo alpha: anAlpha ].
	2 = i ifTrue: [ ^ self initializeRed: lo green: hi blue: up alpha: anAlpha ].
	3 = i ifTrue: [ ^ self initializeRed: lo green: dn blue: hi alpha: anAlpha ].
	4 = i ifTrue: [ ^ self initializeRed: up green: lo blue: hi alpha: anAlpha ].
	5 = i ifTrue: [ ^ self initializeRed: hi green: lo blue: dn alpha: anAlpha ].
	self error: 'implementation error'
]

{ #category : #initialization }
Color >> initializePrivateRed: r green: g blue: b [ 
	"Initialize this color's r, g, and b components to the given values in the range [0..ComponentMax].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r 
		min: ComponentMask
		max: 0) bitShift: RedShift) + ((g 
			min: ComponentMask
			max: 0) bitShift: GreenShift) + (b 
			min: ComponentMask
			max: 0).
	cachedDepth := nil.
	cachedBitPattern := nil
]

{ #category : #initialization }
Color >> initializeRed: r green: g blue: b alpha: anAlpha [ 
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := (((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) + (((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) + ((b * ComponentMax) rounded bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil.
	self setAlpha: anAlpha.
]

{ #category : #initialization }
Color >> initializeRed: r green: g blue: b range: range [ 
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r * ComponentMask // range bitAnd: ComponentMask) bitShift: RedShift) + ((g * ComponentMask // range bitAnd: ComponentMask) bitShift: GreenShift) + (b * ComponentMask // range bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil.
	self setAlpha: 1.0.
]

{ #category : #queries }
Color >> isBitmapFill [
	^false
]

{ #category : #queries }
Color >> isBlack [
	"Return true if the receiver represents black"
	^rgb = 0
]

{ #category : #queries }
Color >> isColor [

	^ true

]

{ #category : #queries }
Color >> isGradientFill [
	^false
]

{ #category : #queries }
Color >> isGray [
	"Return true if the receiver represents a shade of gray"
	^(self privateRed = self privateGreen) and: [self privateRed = self privateBlue]
]

{ #category : #queries }
Color >> isOpaque [
	^ alpha = 255 
]

{ #category : #queries }
Color >> isOrientedFill [
	"Return true if the receiver keeps an orientation (e.g., origin, direction, and normal)"
	^false
]

{ #category : #'self evaluating' }
Color >> isSelfEvaluating [
	^ true
]

{ #category : #queries }
Color >> isSolidFill [
	^true
]

{ #category : #queries }
Color >> isTranslucent [

	^ alpha < 255 
]

{ #category : #queries }
Color >> isTranslucentButNotTransparent [
	"Answer true if this any of this morph is translucent but not transparent."
	
	^ self isTranslucent and: [ self isTransparent not ]
]

{ #category : #queries }
Color >> isTransparent [

	^ alpha = 0
]

{ #category : #wheels }
Color >> lightShades: thisMany [
	"Return an array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.  "
	"(Color red lightShades: 12) inspect"

	^ self class white mix: self shades: thisMany

]

{ #category : #transformations }
Color >> lighter [
	"Answer a lighter shade of this color."

	^ self adjustSaturation: -0.03 brightness: 0.08
]

{ #category : #accessing }
Color >> lightness [
	"Return the HSL lightness of this color, a float in the range [0.0..1.0]."

	^ (self privateMaxComponent + self privateMinComponent) asFloat
		/ ComponentMax / 2.0
]

{ #category : #accessing }
Color >> luminance [
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity."

	^ ((299 * self privateRed) +
	   (587 * self privateGreen) +
	   (114 * self privateBlue)) / (1000 * ComponentMax)

]

{ #category : #wheels }
Color >> mix: color2 shades: thisMany [ 
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"(Color red mix: Color green shades: 12) inspect"
	| redInc greenInc blueInc out rr gg bb |
	thisMany = 1 ifTrue: [ ^ Array with: color2 ].
	redInc := (color2 red - self red) / (thisMany - 1).
	greenInc := (color2 green - self green) / (thisMany - 1).
	blueInc := (color2 blue - self blue) / (thisMany - 1).
	rr := self red.
	gg := self green.
	bb := self blue.
	out := (1 to: thisMany) collect: 
		[ :num | | c | 
		c := self class 
			r: rr
			g: gg
			b: bb.
		rr := rr + redInc.
		gg := gg + greenInc.
		bb := bb + blueInc.
		c ].
	out 
		at: out size
		put: color2.	"hide roundoff errors"
	^ out
]

{ #category : #transformations }
Color >> mixed: proportion with: aColor [ 
	"Mix with another color and do not preserve transpareny.  Only use this for extracting the RGB value and mixing it.  All other callers should use instead: 
	aColor alphaMixed: proportion with: anotherColor
	"
	| frac1 frac2 |
	frac1 := proportion asFloat 
		min: 1.0
		max: 0.0.
	frac2 := 1.0 - frac1.
	^ self class 
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
]

{ #category : #transformations }
Color >> muchDarker [

	^ self alphaMixed: 0.5 with: (ColorRegistry at: #black)

]

{ #category : #transformations }
Color >> muchLighter [

	^ self alphaMixed: 0.233 with: Color white

]

{ #category : #accessing }
Color >> name [
	^ self class registeredNameOf: self   
]

{ #category : #transformations }
Color >> negated [
	"Return an RGB inverted color"
	^Color
		r: 1.0 - self red
		g: 1.0 - self green
		b: 1.0 - self blue
]

{ #category : #transformations }
Color >> orColorUnlike: theOther [
	"If this color is a lot like theOther, then return its complement, otherwide, return self"

	^ (self diff: theOther) < 0.3
		ifTrue: [ theOther negated ]
		ifFalse: [ self ]
]

{ #category : #transformations }
Color >> paler [
	"Answer a paler shade of this color."

	^ self adjustSaturation: -0.09 brightness: 0.09

]

{ #category : #'pixels handling' }
Color >> pixelValueForDepth: d [ 
		 
		"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."
	| basicPixelWord |
	( d < 32 and:[ self isTransparent ] ) ifTrue: [ ^ 0 ].
	basicPixelWord := self basicPixelValueForDepth: d.
	^ d < 32 
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16777215) bitOr: (alpha bitShift: 24) ]
]

{ #category : #'pixels handling' }
Color >> pixelWordFor: depth filledWith: pixelValue [ 
	"Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| halfword |
	depth = 32 ifTrue: [ ^ pixelValue ].
	depth = 16 
		ifTrue: [ halfword := pixelValue ]
		ifFalse: 
			[ halfword := pixelValue * (#(
					65535
					21845
					#-
					4369
					#-
					#-
					#-
					257
				) at: depth)	"replicates at every bit"	"replicates every 2 bits"	"replicates every 4 bits"	"replicates every 8 bits" ].
	^ halfword bitOr: (halfword bitShift: 16)
]

{ #category : #'pixels handling' }
Color >> pixelWordForDepth: depth [ 
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| pixelValue basicPixelWord |
	self isTransparent ifTrue: [ ^ 0 ].
	pixelValue := self pixelValueForDepth: depth.
	basicPixelWord := self pixelWordFor: depth filledWith: pixelValue.
	^ depth < 32 
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16777215) bitOr: (alpha bitShift: 24) ]
]

{ #category : #printing }
Color >> printOn: aStream [
	| name |
	(name := self name).
	name = #unnamed
		ifFalse: [ 
			^ aStream
				nextPutAll: 'Color ';
				nextPutAll: name ].
	self storeOn: aStream
]

{ #category : #private }
Color >> privateAlpha [
	"Private! Return the raw alpha value for opaque. Used only for equality testing."

	^ alpha 
]

{ #category : #private }
Color >> privateBlue [
	"Private! Return the internal representation of my blue component."

	^ rgb bitAnd: ComponentMask
]

{ #category : #private }
Color >> privateGreen [
	"Private! Return the internal representation of my green component."

	^ (rgb bitShift: 0 - GreenShift) bitAnd: ComponentMask
]

{ #category : #private }
Color >> privateMaxComponent [
	^ (self privateRed max: self privateGreen) max: self privateBlue
]

{ #category : #private }
Color >> privateMinComponent [
	^ (self privateRed min: self privateGreen) min: self privateBlue
]

{ #category : #private }
Color >> privateRGB [
	"Private! Return the internal representation of my RGB components."

	^ rgb

]

{ #category : #private }
Color >> privateRed [
	"Private! Return the internal representation of my red component."

	^ (rgb bitShift: 0 - RedShift) bitAnd: ComponentMask
]

{ #category : #transformations }
Color >> quiteBlacker [

	^ self alphaMixed: 0.8 with: (ColorRegistry at: #black)
]

{ #category : #transformations }
Color >> quiteWhiter [

	^ self alphaMixed: 0.6 with: (ColorRegistry at: #white)
]

{ #category : #other }
Color >> raisedColor [
	^ self
]

{ #category : #accessing }
Color >> red [
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self privateRed asFloat / ComponentMax
]

{ #category : #other }
Color >> rgbTriplet [
	"Returns an array composed of the three color float components."
	
	"Color black rgbTriplet >>> #(0.0 0.0 0.0)"
	"Color red rgbTriplet >>> #(1.0 0.0 0.0)"
	^ Array
		with: (self red roundTo: 0.01)
		with: (self green roundTo: 0.01)
		with: (self blue roundTo: 0.01)

]

{ #category : #accessing }
Color >> saturation [
	"Return the saturation of this color, a value between 0.0 and 1.0.
	
	Saturation is a scaling of chroma so that the whole [0..1] range is acceptable for any value of brightness/lightness.
	See #hslSaturation and #hsvSaturation; this method returns HSV saturation for compatibility."

	^ self hsvSaturation
]

{ #category : #'pixels handling' }
Color >> scaledPixelValue32 [
	"Return the alpha scaled pixel value for depth 32"
	| pv32 a b g r |

	pv32 := self pixelWordForDepth: 32 .
	a := (self alpha * 255.0) rounded.
	b := (pv32 bitAnd: 255) * a // 256.
	g := ((pv32 bitShift: -8) bitAnd: 255) * a // 256.
	r := ((pv32 bitShift: -16) bitAnd: 255) * a // 256.
	^ b + (g bitShift: 8) + (r bitShift: 16) + (a bitShift: 24)
]

{ #category : #accessing }
Color >> setAlpha: aFloat [
	alpha := ((255.0 * aFloat) asInteger  min: 255) max: 0.

]

{ #category : #private }
Color >> setHue: hue saturation: saturation brightness: brightness [ 
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."
	| s v hf i f p q t |
	s := (saturation asFloat max: 0.0) min: 1.0.
	v := (brightness asFloat max: 0.0) min: 1.0.

	"zero saturation yields gray with the given brightness"
	s = 0.0 ifTrue: 
		[ ^ self 
			setRed: v
			green: v
			blue: v ].
	hf := hue asFloat.
	(hf < 0.0 or: [ hf >= 360.0 ]) ifTrue: [ hf := hf - ((hf quo: 360.0) asFloat * 360.0) ].
	hf := hf / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"
	p := (1.0 - s) * v.
	q := (1.0 - (s * f)) * v.
	t := (1.0 - (s * (1.0 - f))) * v.
	0 = i ifTrue: 
		[ ^ self 
			setRed: v
			green: t
			blue: p ].
	1 = i ifTrue: 
		[ ^ self 
			setRed: q
			green: v
			blue: p ].
	2 = i ifTrue: 
		[ ^ self 
			setRed: p
			green: v
			blue: t ].
	3 = i ifTrue: 
		[ ^ self 
			setRed: p
			green: q
			blue: v ].
	4 = i ifTrue: 
		[ ^ self 
			setRed: t
			green: p
			blue: v ].
	5 = i ifTrue: 
		[ ^ self 
			setRed: v
			green: p
			blue: q ].
	self error: 'implementation error'
]

{ #category : #private }
Color >> setPrivateRed: r green: g blue: b [ 
	"Initialize this color's r, g, and b components to the given values in the range [0..ComponentMax].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r 
		min: ComponentMask
		max: 0) bitShift: RedShift) + ((g 
			min: ComponentMask
			max: 0) bitShift: GreenShift) + (b 
			min: ComponentMask
			max: 0).
	cachedDepth := nil.
	cachedBitPattern := nil
]

{ #category : #private }
Color >> setRGB: rgb0 [ 
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := rgb0
]

{ #category : #private }
Color >> setRed: r green: g blue: b [ 
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := (((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) + (((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) + ((b * ComponentMax) rounded bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil
]

{ #category : #private }
Color >> setRed: r green: g blue: b range: range [ 
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r * ComponentMask // range bitAnd: ComponentMask) bitShift: RedShift) + ((g * ComponentMask // range bitAnd: ComponentMask) bitShift: GreenShift) + (b * ComponentMask // range bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil
]

{ #category : #printing }
Color >> shortPrintString [
	"Return a short (but less precise) print string for use where space is tight."

	^ String streamContents: [ :s |
				s
					nextPutAll: '(';
					nextPutAll: self class name;
					nextPutAll: ' r: '.
				(self red roundTo: 0.01) printOn: s.
				s nextPutAll: ' g: '.
				(self green roundTo: 0.01) printOn: s.
				s nextPutAll: ' b: '.
				(self blue roundTo: 0.01) printOn: s.
				s nextPutAll: ')' ]
]

{ #category : #transformations }
Color >> slightlyDarker [

	^ self adjustBrightness: -0.03

]

{ #category : #transformations }
Color >> slightlyLighter [

	^ self adjustSaturation: -0.01 brightness: 0.03
]

{ #category : #transformations }
Color >> slightlyWhiter [

	^ self alphaMixed: 0.85 with: (ColorRegistry at: #white)

]

{ #category : #printing }
Color >> storeArrayOn: aStream [

	aStream nextPutAll: '#('.
	self storeArrayValuesOn: aStream.
	aStream nextPutAll: ') '

]

{ #category : #printing }
Color >> storeArrayValuesOn: aStream [
	self isTransparent ifTrue: [ ^ aStream space. ].

	(self red roundTo: 0.001) storeOn: aStream.
	aStream space.
	(self green roundTo: 0.001) storeOn: aStream.
	aStream space.
	(self blue roundTo: 0.001) storeOn: aStream.
	aStream space.
	(self alpha roundTo: 0.001) storeOn: aStream.


]

{ #category : #printing }
Color >> storeOn: aStream [
	self isTransparent
		ifTrue: [ ^ aStream nextPutAll: '(Color transparent)' ].
	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' r: ';
		print: self red;
		nextPutAll: ' g: ';
		print: self green;
		nextPutAll: ' b: ';
		print: self blue;
		nextPutAll: ' alpha: ';
		print: self alpha;
		nextPutAll: ')'
]

{ #category : #transformations }
Color >> twiceDarker [
	"Answer a significantly darker shade of this color."

	^ self adjustBrightness: -0.15
]

{ #category : #transformations }
Color >> twiceLighter [
	"Answer a significantly lighter shade of this color."

	^ self adjustSaturation: -0.06 brightness: 0.15
]

{ #category : #copying }
Color >> veryDeepCopyWith: deepCopier [
	"Return self.  I am immutable in the Morphic world.  Do not record me."
]

{ #category : #transformations }
Color >> veryMuchLighter [

	^ self alphaMixed: 0.1165 with: (ColorRegistry at: #white)

]

{ #category : #wheels }
Color >> wheel: thisMany [ 
	"An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self.  Array is of length thisMany.  Very useful for displaying color based on a variable in your program.  "
	"(Color wheel: 8) inspect"
	"(Color wheel: 8) withIndexDo: [:c :i | Display fill: (i*10@20 extent: 10@20) fillColor: c]"
	
	| sat bri step hue |
	sat := self saturation.
	bri := self brightness.
	hue := self hue.
	step := 360.0 / (thisMany max: 1).
	^ (1 to: thisMany) collect: 
		[ :num | | c | 
		c := self class 
			h: hue
			s: sat
			v: bri.	"hue is taken mod 360"
		hue := hue + step.
		c ]

]

{ #category : #transformations }
Color >> whiter [

	^ self alphaMixed: 0.8333 with: (ColorRegistry at: #white)

]
