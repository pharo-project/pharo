initialization
registerListShortcutsFor: aWidget

	aWidget bindKeyCombination: PharoShortcuts current browseShortcut toAction: [ self browseMethod ].
	aWidget bindKeyCombination: PharoShortcuts current inspectItShortcut toAction: [ self inspectMethod ].
	aWidget bindKeyCombination: PharoShortcuts current browseImplementorsShortcut toAction: [ self browseMessages ].
	aWidget bindKeyCombination: PharoShortcuts current browseSendersShortcut toAction: [ self browseSendersOfMessage ].
	aWidget bindKeyCombination: PharoShortcuts current browseReferencesShortcut toAction: [ self browseClassRefs ].
	aWidget bindKeyCombination: PharoShortcuts current browseVersionsShortcut toAction: [ self browseVersions ].
	aWidget bindKeyCombination: PharoShortcuts current removeShortcut toAction: [ self removeMethods ].