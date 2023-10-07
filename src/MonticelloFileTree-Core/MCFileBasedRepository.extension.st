Extension { #name : 'MCFileBasedRepository' }

{ #category : '*MonticelloFileTree-Core' }
MCFileBasedRepository >> packageDescriptionsFromReadableFileNames [
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
]

{ #category : '*MonticelloFileTree-Core' }
MCFileBasedRepository >> retrieveVersionsWithPackageNames: packageNames [
	| packageDescriptions |
	packageDescriptions := self packageDescriptionsFromReadableFileNames.
	packageNames addAll: (packageDescriptions collect: [ :packageDescription | packageDescription first ]).
	^ packageDescriptions select: [ :each | (each at: 3) isNumber ]
]
