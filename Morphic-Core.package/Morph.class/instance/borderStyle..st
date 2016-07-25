accessing
borderStyle: newStyle
	newStyle = self borderStyle
		ifTrue: [ ^ self ].
	(self canDrawBorder: newStyle)
		ifFalse: [ 
			"Replace the suggested border with a simple one"
			^ self borderStyle: (BorderStyle width: newStyle width color: (newStyle trackColorFrom: self) color) ].
	self assureExtension.
	self extension borderStyle: newStyle.
	self changed