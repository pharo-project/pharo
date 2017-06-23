writing
writeTraitDefinition: definition to: classPath
	self
		writeInDirectoryName: classPath
		fileName: 'README'
		extension: '.md'
		visit: [ self writeClassComment: definition ].
	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertyFileExtension
		visit: [ self writeTraitDefinition: definition ]