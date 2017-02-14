protocol
showHierarchy: aBoolean forClass: class

	(showHierarchy := aBoolean)
		ifTrue: [ class ifNotNil: [ self setCachedHierarchyClass: class ] ]