Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Gofer-Deprecated' }
MCFileBasedRepository >> goferReferences [
	| versionNames |
	versionNames := [ self allVersionNames ]
		on: MCRepositoryError
		do: [ :error | ^ GoferRepositoryError signal: error messageText repository: self ].
	^ versionNames collect: [ :each | GoferResolvedReference name: each repository: self ]
]
