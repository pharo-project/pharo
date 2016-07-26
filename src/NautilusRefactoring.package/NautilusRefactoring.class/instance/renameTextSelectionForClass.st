source
renameTextSelectionForClass
	| selectedInterval selection class |
	class := self model selectedClass.
	selection := self model contentSelection.
	selectedInterval := self model selectionInterval.
	selectedInterval isEmpty
		ifTrue: [ ^ self model flashSourceCodeArea ].
	(selection includes: Character space)
		ifTrue: [ ^ self model flashSourceCodeArea ].	"Try to rename the superclass"
	selectedInterval first = 1
		ifTrue: [ self changeSuperclassOf: class ].	"Rename the selected class"
	selection = class name
		ifTrue: [ self renameClass: class ].	"Rename a variable"
	self renameVariable: selection