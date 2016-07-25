instance creation
newRow
	^ GLMInteractableBrick new
		enable;
		enableDoubleClick;
		selectedColor: self themer bindingThemer selectedColor;
		vShrinkWrap;
		hSpaceFill;
		padding: self themer bindingThemer padding;
		paddingRight: 0