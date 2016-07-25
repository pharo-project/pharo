protocol
showHierarchy: aBoolean

	(showHierarchy := aBoolean)
		ifTrue: [ self selectedClass ifNotNil: [ :class | self setCachedHierarchyClass: class ] ]