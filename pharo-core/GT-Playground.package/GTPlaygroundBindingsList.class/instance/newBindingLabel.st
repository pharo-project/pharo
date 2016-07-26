instance creation
newBindingLabel

	^ GLMLabelBrick new
		hSpaceFill: 50;
		vShrinkWrap;
		textPosition: #leftCenter;
		vAlign: #center;
		padding: { 0 . self themer bindingThemer padding }