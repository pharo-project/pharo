selected
selectedFontIndex
	| font textStyleName family |
	
	selectedFontIndex ifNotNil: [^selectedFontIndex].
	selectedFontIndex := 0.
	font := (getSelector isSymbol and:[target notNil])
		ifTrue:[target perform: getSelector]
		ifFalse:[getSelector].
	self setStyleValuesFrom: font.
	(font isKindOf: AbstractFont)
		ifTrue:[
			pointSize := font pointSize.
			textStyleName := font textStyleName.
			family := self fontList detect:[:f | f familyName = textStyleName] ifNone:[].
			selectedFontIndex := self fontList indexOf: family ifAbsent:[0]].
	self selectedFontIndex: selectedFontIndex.
	^selectedFontIndex