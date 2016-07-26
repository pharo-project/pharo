instance creation
buildHierarchyForClass: aClass
	| tupleBlock heirarchy indentDepth |
	
	indentDepth := 0.
	
	heirarchy := OrderedCollection new.
	aClass allSuperclasses reversed do: [ :superClass | 
		heirarchy add: (SortHierarchicallyIndentNode on: superClass indentBy: indentDepth). 
		indentDepth := indentDepth + 1
	].
	
	tupleBlock := nil.
	tupleBlock := [ :aTuple :indent | 
		heirarchy add: (SortHierarchicallyIndentNode on: aTuple first indentBy: indent). 
		aTuple second do: [ :tuple | tupleBlock value: tuple value: indent + 1 ]
	].
	tupleBlock value: aClass hierarchyClassTree value: indentDepth.
		
	^ self new
		setHierarchy: heirarchy;
		yourself