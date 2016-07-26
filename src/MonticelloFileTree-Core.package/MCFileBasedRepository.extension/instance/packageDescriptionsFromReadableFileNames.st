*monticellofiletree-core
packageDescriptionsFromReadableFileNames
	^ self readableFileNames
		collect: [ :each | 
			| name |
			name := (each copyUpToLast: $.) copyUpTo: $(.
			name last isDigit
				ifFalse: [ 
					{name.
					''.
					''.
					each} ]
				ifTrue: [ 
					| packageName author versionNumber |
					packageName := name copyUpToLast: $-.
					author := (name copyAfterLast: $-) copyUpTo: $..
					versionNumber := ((name copyAfterLast: $-) copyAfter: $.) asInteger ifNil: [ 0 ].
					{packageName.
					author.
					versionNumber.
					each} ] ]