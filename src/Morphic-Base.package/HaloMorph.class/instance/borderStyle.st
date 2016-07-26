accessing
borderStyle
	"Answer the border style to use for the receiver.
	Depends on the target and on some settings."
	
	^(target notNil and: [self showBoundsInHalo and: [target isWorldMorph not]])
		ifTrue: [super borderStyle]
		ifFalse: [SimpleBorder width: 0 color: Color transparent]