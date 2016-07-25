user interface
rootItems
	^ (MCWorkingCopy allManagers sort: [ :a :b | 
		"Sort alphabetically, putting modified packages first in the list. Copy/pasted from MCWorkingCopyBrowser>>#orderSpecs"
		(a modified = b modified) 
			ifTrue: [ a package name <= b package name  ]
			ifFalse: [ a modified ]
		])