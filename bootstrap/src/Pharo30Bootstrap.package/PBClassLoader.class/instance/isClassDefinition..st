class-loading
isClassDefinition: aString

	^ aString beginsWith: '| newClass |
	newClass := PharoClassInstaller make: [ :builder |
		builder
			superclass: '.