initialization
initialize
	super initialize.
	
	fontSize := self themer textFontSize.
	familyName := self themer textFamilyName.
	text := ''.
	
	textPosition := #leftCenter.
	
	self
		hShrinkWrap;
		vShrinkWrap.
	self updateFont.
	