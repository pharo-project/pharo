private
loadClassesListElements
	^ self showHierarchy
		ifTrue: [ self loadClassHierarchyListElements ]
		ifFalse: [ self loadPackageClassListElements ]