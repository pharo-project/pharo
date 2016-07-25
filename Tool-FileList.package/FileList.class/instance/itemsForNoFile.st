file list menu
itemsForNoFile

	| services |
	services := OrderedCollection new.
	services addAll: (self itemsForDirectory: (self isFileSelected ifFalse: [ self directory ] ifTrue: [])).
	^ services

		