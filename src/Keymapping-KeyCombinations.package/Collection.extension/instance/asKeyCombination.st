*Keymapping-KeyCombinations
asKeyCombination
	| shortcut |
	self size = 1 ifTrue: [ ^self first asKeyCombination ].

	shortcut := KMKeyCombinationSequence new.
	self do: [ :each | shortcut addShortcut: each asKeyCombination ].
	^shortcut.