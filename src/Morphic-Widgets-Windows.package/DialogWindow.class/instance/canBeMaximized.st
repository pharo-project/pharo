testing
canBeMaximized
	"Answer whether we are not we can be maximised."

	^self isResizeable
		ifTrue: [super canBeMaximized]
		ifFalse: [false]