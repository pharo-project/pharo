interaction delegation
critiqueSelected: aCritique
	aCritique sourceAnchor providesInterval ifTrue: [ 
		self model codeTextMorph
			setSelection: aCritique sourceAnchor interval;
			takeKeyboardFocus ]