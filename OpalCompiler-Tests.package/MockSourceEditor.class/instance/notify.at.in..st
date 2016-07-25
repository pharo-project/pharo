as yet unclassified
notify: aString at: anInteger in: aStream

	| pos |
	pos := self selectionInterval notEmpty
		ifTrue: [selectionStart + anInteger - 1 ]
		ifFalse: [anInteger].
	self insertAndSelect: aString at: (pos max: 1)