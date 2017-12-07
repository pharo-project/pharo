class-loading
isClassDefinition: aString

	^ aString beginsWith: '| newClass |
	newClass := ShiftClassInstaller make: [ :builder |
		builder
			superclass: '.