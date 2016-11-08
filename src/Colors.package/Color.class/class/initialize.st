initialize-release
initialize
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