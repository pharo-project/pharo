testing
isPossibleBaseline

	| vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(vrsn allPackagesForSpecNamed: (self loadListForVersion: vrsn)) do: [:pkg |
		pkg workingCopy == nil ifTrue: [ ^false ]].
	^true