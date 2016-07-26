accessing
at: index put: aColor
	^super at: index put: (aColor pixelWordForDepth: 32).