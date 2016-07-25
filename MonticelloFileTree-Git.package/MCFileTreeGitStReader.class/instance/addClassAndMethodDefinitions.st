utilities
addClassAndMethodDefinitions
	(self zip members select: [ :m | m isDirectory and: [ m splitFileName last endsWith: '.trait' ] ])
		do: [ :entry | self addTraitAndMethodDefinitionsFromEntry: entry ].
	(self zip members select: [ :m | m isDirectory and: [ m splitFileName last endsWith: '.class' ] ])
		do: [ :entry | self addClassAndMethodDefinitionsFromEntry: entry ].
	(self zip members select: [ :m | m isDirectory and: [ m splitFileName last endsWith: '.extension' ] ])
		do: [ :entry | self addExtensionClassAndMethodDefinitionsFromEntry: entry ]