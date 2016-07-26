accessing
contentsOfArea: aRectangle into: aForm
	| bb |
	self flush.
	bb := BitBlt toForm: aForm.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: (aRectangle left + origin x); sourceY: (aRectangle top + origin y);
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm