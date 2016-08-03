accessing
logFilesIn: aDirectory
	aDirectory exists ifFalse: [ ^Array empty ].
	
	^(aDirectory files
		select: [ :each | each extension = 'ombu'  ])
		sorted: [ :a :b | a modificationTime >= b modificationTime ]