*Polymorph-Widgets
labelHeight
	"Answer the height for the window label.  The standard behavior is at bottom; a hook is provided so that models can stipulate other heights, in support of various less-window-looking demos.
	If no label answer the class border width instead."

	label ifNil: [^self isEmbedded ifTrue: [0] ifFalse: [self class borderWidth]].
		
	(model notNil and: [model respondsTo: #desiredWindowLabelHeightIn:]) ifTrue: [
		(model desiredWindowLabelHeightIn: self) ifNotNil: [:h | ^h]].

	^(label height + (self class borderWidth * 2)) max:
		(collapseBox ifNotNil: [collapseBox height] ifNil: [10])