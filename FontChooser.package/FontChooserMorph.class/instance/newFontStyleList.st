interface building
newFontStyleList 

	| widget minWidth requiredWidth |
	widget := PluggableListMorph
		on: self model
		list: #fontStyleListStrings
		selected: #selectedFontStyleIndex
		changeSelected: #selectedFontStyleIndex:.
		
	minWidth := widget font widthOfStringOrText: 'Condensed Extra Bold Oblique' "long, but not the longest".
	requiredWidth := self
		widthRequiredFor: [ :each | widget font widthOfStringOrText: each styleName ]
		from: model fontStyleList
		noLessThan: minWidth.

	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			width: requiredWidth + widget scrollBarThickness + (widget font widthOfStringOrText: '  ');
			yourself.