*Versionner-Core-Model
configurationBaseName
	"Return the baseName for this configuration class name."
	
	^ (self indexOfSubCollection: 'ConfigurationOf') = 0
		ifTrue: [ self ]
		ifFalse: [ self copyFrom: ('ConfigurationOf' size + 1) to: self size]