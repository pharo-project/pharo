git querying
gitVersionsForDefinition: aMCDefinition in: aPackageName
	"Query git for all the versions related to that method definition."

	| fileDirectory methodFile |
	fileDirectory := self fileUtils
		directoryFromPath:
			aMCDefinition className
				,
					(aMCDefinition isExtensionMethod
						ifTrue: [ '.extension' ]
						ifFalse: [ '.class' ])
		relativeTo: (self fileUtils directoryFromPath: aPackageName , '.package' relativeTo: directory).
	methodFile := (aMCDefinition classIsMeta
		ifTrue: [ 'class' ]
		ifFalse: [ 'instance' ]) , '/' , (MCFileTreeStCypressWriter new fileNameForSelector: aMCDefinition selector) , '.st'.
	(self fileUtils directoryExists: fileDirectory)
		ifFalse: [ NotFound signalFor: fileDirectory ].
	^ (self gitCommand: #('log' '--format=%H' '--') , {methodFile} in: fileDirectory) substrings
		collect: [ :e | ByteArray readHexFrom: e ]
