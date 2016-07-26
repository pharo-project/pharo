items creation
buildDescriptionTextArea
	^ self textModel newScrolledText
		beForPlainText;
		beWrapped;
		beReadOnly;
		enabled: false;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself