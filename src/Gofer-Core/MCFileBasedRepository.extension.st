Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Gofer-Core' }
MCFileBasedRepository >> goferReferences [
	| versionNames |
	versionNames := [ self allVersionNames ]
		on: MCRepositoryError
		do: [ :error | ^ GoferRepositoryError signal: error messageText repository: self ].
	^ versionNames collect: [ :each | GoferResolvedReference name: each repository: self ]
]

{ #category : '*Gofer-Core' }
MCFileBasedRepository >> packageNamed: aName ifAbsent: aBlock [

	| versionNames |
	versionNames := [ self allVersionNames ]
		                on: MCRepositoryError
		                do: [ :error |
			                ^ GoferRepositoryError
				                  signal: error messageText
				                  repository: self ].
	^ self versionFrom: (versionNames
			   detect: [ :e | e packageName = aName ]
			   ifNone: [ ^ aBlock value ])
]
