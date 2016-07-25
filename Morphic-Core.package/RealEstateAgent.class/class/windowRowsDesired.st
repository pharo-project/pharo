settings
windowRowsDesired
	"Answer how many separate horizontal rows of windows are wanted."
	^ (self usedStrategy = #staggerFor:initialExtent:world:)
		ifTrue: [1]
		ifFalse: [self maximumUsableArea height > 480
				ifTrue: [2]
				ifFalse: [1]] 