initialization
doFromMethodContainer: aMethodContainer
	category := aMethodContainer category.
	traitComposition := aMethodContainer traitComposition asExportedLiteral.
	classTraitComposition := aMethodContainer theMetaClass traitComposition
		asExportedLiteral.

	methods := aMethodContainer localMethods
		select: [ :e | e isExtension not ]
		thenCollect: [ :e | HEMethod for: e ].

	classSideMethods := aMethodContainer theMetaClass localMethods
		select: [ :e | e isExtension not ]
		thenCollect: [ :e | HEMethod for: e ]