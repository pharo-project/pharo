shortcuts
registerClassShortcuts: aWidget

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self selectedClass ifNotNil:[ self browseMethodFull ]].
	aWidget bindKeyCombination: $d meta toAction: [ self selectedClass ifNotNil:[ self forgetClass ]].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self selectedClass ifNotNil:[ self removeClass ]].