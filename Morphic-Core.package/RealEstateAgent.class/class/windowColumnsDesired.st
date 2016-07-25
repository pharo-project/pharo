settings
windowColumnsDesired
	"Answer how many separate vertical columns of windows are
	wanted."
	^ (self usedStrategy = #staggerFor:initialExtent:world:)
		ifTrue: [1]
		ifFalse: [self maximumUsableArea width > 640
				ifTrue: [2]
				ifFalse: [1]]