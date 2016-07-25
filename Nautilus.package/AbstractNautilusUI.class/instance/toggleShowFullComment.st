buttons behavior
toggleShowFullComment

	self showComment ifFalse: [ ^ self ].
	sourceCodePanel removeAllMorphs.
	sourceCodePanel 
		addMorph: commentWidget 
		fullFrame: LayoutFrame identity.
