accessing
zipForDefinition: aMCDefinition
	"We suppose we are trying to load a specific method definition. Recover the relevant archive as needed."

	"Ask in the archive, the method file, and the class properties file."

	| classDirectory methodFile classPropertiesFile |
	self assert: aMCDefinition isMethodDefinition.
	zip := ZipArchive new.
	classDirectory := packageDirectory , self slash , aMCDefinition className
		,
			(aMCDefinition isExtensionMethod
				ifTrue: [ '.extension' ]
				ifFalse: [ '.class' ]).
	methodFile := classDirectory , self slash
		,
			(aMCDefinition classIsMeta
				ifTrue: [ 'class' ]
				ifFalse: [ 'instance' ]) , self slash , (MCFileTreeStCypressWriter new fileNameForSelector: aMCDefinition selector)
		, '.st'.
	classPropertiesFile := classDirectory , self slash , 'properties.json'.
	zip
		readFrom:
			(self
				gitCommand:
					{'archive'.
					'--format=zip'.
					(commitID hex).
					methodFile.
					classPropertiesFile}
				in: stream) asByteArray readStream.
	^ zip