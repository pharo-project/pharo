repositories
directory: aDirectoryOrString
	"Add a file-system repository at aDirectoryOrString."

	| repository |
	repository := (aDirectoryOrString isString and: [ aDirectoryOrString endsWith: '*' ])
		ifTrue: [ 
			MCSubDirectoryRepository new
				directory: aDirectoryOrString allButLast asFileReference;
				yourself ]
		ifFalse: [ 
			(MCDirectoryRepository new)
				directory:
						(aDirectoryOrString isString
								ifTrue: [ aDirectoryOrString asFileReference ]
								ifFalse: [ aDirectoryOrString ]);
				yourself ].
	self repository: repository