*System-CommandLine-TextSupport
color256: aColor background: isBackground
	"use the resources for colors 0-15 - usually more-or-less a reproduction of the standard ANSI colors, but possibly more pleasing shades

	colors 16-231 are a 6x6x6 color cube

	colors 232-255 are a grayscale ramp, intentionally leaving out black and white"
	
	self csiEscape.

	outStream nextPutAll: (isBackground 
		ifFalse: [ '38;5;' ]
		ifTrue:  [ '48;5;' ]).
		
	outStream 
		print: aColor closestXTermPixelValue;
		nextPut: $m.