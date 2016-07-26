checkbox-brick-accessing
checkBrick: anObject
	checkBrick := anObject.
	self box
		borderColor: self themer checkboxBorderColor;
		borderWidth: self themer checkboxBorderWidth;
		vAlign: #center;
		checkedImage: self themer checkboxCheckedImage;
		width: self themer checkboxWidth;
		height: self themer checkboxHeight.
		
	self addBrickFirst: self box