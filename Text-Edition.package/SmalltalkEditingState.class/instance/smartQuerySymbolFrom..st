selector-chooser
smartQuerySymbolFrom: aTextMorph
	| sel pref caret nextPref |
	sel := aTextMorph editor selectionInterval.
	aTextMorph editor selectPrecedingIdentifier.
	pref := aTextMorph selection asString.
	selectorChooser := SelectorChooserMorph popUpPrefix: pref from: aTextMorph.
	nextPref := selectorChooser 
		ifNotNil: [ selectorChooser nextPrefixWith: pref ].
	nextPref 
		ifNil: [aTextMorph editor selectInterval: sel]
		ifNotNil: [	selectorChooser prefix: nextPref.
			aTextMorph editor selectPrecedingIdentifier.
			aTextMorph editor replaceSelectionWith: nextPref.
			caret := aTextMorph editor selectionInterval first + nextPref size.
			aTextMorph editor selectInterval: (caret to: caret - 1)]