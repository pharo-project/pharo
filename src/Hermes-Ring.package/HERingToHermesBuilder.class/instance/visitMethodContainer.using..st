visiting
visitMethodContainer: aMethodContainer using: aHEClass
	aHEClass category: aMethodContainer category.
	aHEClass traitComposition: aMethodContainer traitComposition asExportedLiteral.
	aHEClass classTraitComposition: aMethodContainer theMetaClass traitComposition asExportedLiteral.

	aHEClass methods: (aMethodContainer localMethods collect: [ :e | e acceptVisitor: self ]) asArray.
	aHEClass classSideMethods: (aMethodContainer theMetaClass localMethods collect: [ :e | e acceptVisitor: self])