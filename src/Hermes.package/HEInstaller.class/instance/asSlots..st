converting
asSlots: aString
	| trimmed |
	trimmed := aString trimBoth.

	trimmed isEmpty
		ifTrue: [ ^ #() ].

	^ (trimmed splitOn: ' ') collect: [ :e | e asSymbol asSlot ] as: Array