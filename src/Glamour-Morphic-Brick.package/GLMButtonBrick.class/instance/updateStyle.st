button-updating
updateStyle

	self borderWidth: self themer buttonBorderWidth.
	self borderColor: self themer buttonBorderColor.
	self padding: self themer buttonPadding.
	self normalColor: self themer buttonBackgroundColor.
	self selectedColor: self themer buttonSelectedColor.
	self pressedColor: self themer buttonPressedColor.
	self minWidth: self themer buttonMinWidth.
	self minHeight: self themer buttonMinHeight.
	self doLayoutForce
	