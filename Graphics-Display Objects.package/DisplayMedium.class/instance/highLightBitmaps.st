private
highLightBitmaps
	"Quickly return a Bitblt-ready raw colorValue for highlighting areas."
	^HighLightBitmaps ifNil: [self initializeHighLights ].
