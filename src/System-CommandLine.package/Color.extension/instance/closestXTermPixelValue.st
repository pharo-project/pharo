*System-CommandLine
closestXTermPixelValue
	| index |
	"Return the nearest approximation to this color for 256 xterm color."

	self saturation < 0.2 ifTrue: [
		"colors 232-255 are a grayscale ramp, intentionally leaving out black and white"
		index := (self green * 25) asInteger.
		index = 0 ifTrue: [ ^ 0 ].
		index = 25 ifTrue: [ ^ 16 ].
		^ index + 232 ].
	
	"compute nearest entry in the 6*6*6 color cube"
	^ 16 + (self red * 6*6*5) asInteger + (self green * 6*5) asInteger + (self blue * 5) asInteger