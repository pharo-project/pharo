spec creation
createVersion: versionString
	| version  |
	[ version := project version: versionString ]
		on: MetacelloVersionDoesNotExistError
		do: [ :ex | 
			| versionSpec |
			versionSpec := self createVersionSpec: versionString.
			version := versionSpec createVersion.
			project map == nil
				ifTrue: [ project map: Dictionary new ].
			project map at: versionString put: version ].
	^version