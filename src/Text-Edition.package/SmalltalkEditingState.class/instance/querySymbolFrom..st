selector-chooser
querySymbolFrom: aTextMorph
	| pref |

	aTextMorph editor hasSelection
		ifFalse: [aTextMorph editor selectPrecedingIdentifier].
	pref := String streamContents: [:strm |
		aTextMorph selection asString trimBoth do: [:c | c isSeparator ifFalse: [strm nextPut: c]]].
	selectorChooser := SelectorChooserMorph popUpPrefix: pref from: aTextMorph.
