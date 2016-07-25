other
newDepth: pixelSize 
	"
	Display newDepth: 8.
	Display newDepth: 1
	"
	(self supportsDisplayDepth: pixelSize)
		ifFalse: [^ self inform: 'Display depth ' , pixelSize printString , ' is not supported on this system'].
	self newDepthNoRestore: pixelSize.
	self restore