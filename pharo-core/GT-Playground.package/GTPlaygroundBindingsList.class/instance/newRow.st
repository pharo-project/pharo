instance creation
newRow

	^ GLMInteractableBrick new
		enable;
		selectedColor: self themer bindingThemer selectedColor;
		vShrinkWrap;
		hSpaceFill;
		padding: { self themer bindingThemer padding . 0 . self themer bindingThemer padding + self themer bindingThemer borderWidth };
		borderColor: self themer bindingThemer borderColor