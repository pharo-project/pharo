button-brick-accessing
labelBrick: anObject
	labelBrick := anObject.
	
	self label
		vShrinkWrap;
		hShrinkWrap;
		text: 'button';
		textPosition: #center;
		vAlign: #center;
		hAlign: #center.
	
	self addBrickBack: self label