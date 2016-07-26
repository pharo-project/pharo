shortcuts
registerMethodShortcuts: aWidget

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self selectedSelector ifNotNil:[ self browseMethodFull ]].
	aWidget bindKeyCombination: $d meta toAction: [ self selectedSelector ifNotNil:[ self forgetMessage ]].
	aWidget bindKeyCombination: PharoShortcuts current browseImplementorsShortcut toAction: [ self selectedSelector ifNotNil:[ self browseImplementorsOfMessages ]].
	aWidget bindKeyCombination: PharoShortcuts current browseSendersShortcut toAction: [ self selectedSelector ifNotNil:[ self browseSendersOfMessages ]].
	aWidget bindKeyCombination: PharoShortcuts current browseVersionsShortcut toAction: [ self selectedSelector ifNotNil:[ self browseVersions ]].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self selectedSelector ifNotNil:[ self removeMessage ]].