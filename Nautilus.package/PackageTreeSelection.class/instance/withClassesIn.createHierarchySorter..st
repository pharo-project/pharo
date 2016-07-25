converting
withClassesIn: anEnvironment createHierarchySorter: aHierarchyClass 
	| classes defined extensions |
	classes := self definedClasses collect: #theNonMetaClass.
	anEnvironment isSystem
		ifFalse: [ classes := classes intersection: anEnvironment classes ].

	defined := aHierarchyClass buildHierarchyForClasses: classes.
	
	classes := self extensionClasses collect: #theNonMetaClass.
	anEnvironment isSystem
		ifFalse: [ classes := classes intersection: anEnvironment classes ].
	
	extensions := aHierarchyClass buildHierarchyForClasses: classes asSet.

	^ defined, extensions