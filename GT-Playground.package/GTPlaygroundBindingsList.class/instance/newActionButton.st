instance creation
newActionButton
	|margin padding|
	margin := self themer bindingThemer padding + self themer bindingThemer borderWidth.
	padding := (margin / 2) rounded.
	
	^ GLMHintableActionButtonBrick new
		padding: padding;
		visible: false;
		margin: {0 . padding};
		marginRight: margin;
		hShrinkWrap;
		vAlign: #center;
		pressedColor: self themer buttonPressedColor