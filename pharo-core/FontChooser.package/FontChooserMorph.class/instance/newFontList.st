interface building
newFontList 

	| widget requiredWidth |
	widget := PluggableListMorph
		on: self model
		list: #fontListStrings
		selected: #selectedFontIndex
		changeSelected: #selectedFontIndex:.

	requiredWidth := self
		widthRequiredFor: [ :each | widget font widthOfStringOrText: each familyName ]
		from: model fontList
		noLessThan: 20.

	widget	
		color: Color white;
		borderInset;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		width: requiredWidth + widget scrollBarThickness + (widget font widthOfStringOrText: '  ').
		
	self model announcer when: #fontsUpdated send: #verifyContents to: widget.
	
	^ widget.