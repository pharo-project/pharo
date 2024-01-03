Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Gofer-Deprecated' }
MCFileBasedRepository >> goferReferences [
	| versionNames |
	versionNames := [ self allVersionNames ]
		on: MCRepositoryError
		do: [ :error | ^ GoferRepositoryError signal: error messageText repository: self ].
	^ versionNames collect: [ :each | GoferResolvedReference name: each repository: self ]
]

{ #category : '*Gofer-Deprecated' }
MCFileBasedRepository >> packageNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [

	| versionNames versionName |
	versionNames := [ self packageDescriptionsFromReadableFileNames ]
		                on: MCRepositoryError
		                do: [ :error |
			                ^ GoferRepositoryError
				                  signal: error messageText
				                  repository: self ].
	versionName := versionNames
		               detect: [ :e | e first = aName ]
		               ifNone: [ nil ].
	versionName ifNil: [ ^ absentBlock value ].
	^ presentBlock value: (self versionFromFileNamed: versionName last)
]
