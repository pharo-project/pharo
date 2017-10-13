accessing
focusBounds
	"Answer the bounds for drawing the focus indication."

	^(self bounds width < 6 or: [self bounds height < 6])
		ifTrue: [super focusBounds]
		ifFalse: [super focusBounds insetBy: (2@2 corner: 2@0)]