multi-selection
listSelectionAt: aNumber put: aBoolean
	| item |
	item := self items at: aNumber.
	aBoolean
		ifTrue: [self kept add: item ]
		ifFalse: [self kept remove: item ifAbsent: []]