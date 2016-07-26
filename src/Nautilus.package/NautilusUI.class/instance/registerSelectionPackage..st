private dispatch
registerSelectionPackage: aSelection

	| selectedClass |
	
	selectedClass := self selectedClass.

	selectedClass
		ifNil: [ self model package: aSelection ]
		ifNotNil: [ self model package: aSelection class: selectedClass ].
		
	self packageWidget selectPackage: aSelection item.