basic-completion
selectorChooseQuerySymbol
	| pref |
	self hasSelection
		ifFalse: [ self selectPrecedingIdentifier ].
	pref := String
		streamContents: [ :strm | 
			self selection asString trimBoth
				do: [ :c | 
					c isSeparator
						ifFalse: [ strm nextPut: c ] ] ].
	selectorChooser := RubSelectorChooserMorph popUpPrefix: pref from: textArea