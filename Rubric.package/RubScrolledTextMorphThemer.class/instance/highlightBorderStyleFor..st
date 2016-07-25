border-styles
highlightBorderStyleFor: aTextArea
	^ BorderStyle inset 
		baseColor: Smalltalk ui theme selectionColor;
		width: 2