menu
showOrHideHandles
	self showingHandles
		ifTrue:	[self removeHandles]
		ifFalse:	[self addHandles]