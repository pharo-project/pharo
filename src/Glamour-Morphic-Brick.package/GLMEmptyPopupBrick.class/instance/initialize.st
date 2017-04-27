initialization
initialize
	super initialize.
	
	anchorBrick := Morph new extent: 1@1.
	popupLayout := self defaultPopupLayout.
	closer := self defaultCloser.
	inset := self themer popupThemer inset asMargin.
	spacing := self themer popupThemer spacing.

	self
		initializeListeners;
		useVerticalLinearLayout;
		vShrinkWrap;
		hShrinkWrap;
		enable;
		initializeLayout