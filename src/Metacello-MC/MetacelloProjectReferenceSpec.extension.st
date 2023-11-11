Extension { #name : 'MetacelloProjectReferenceSpec' }

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> loadUsing: aLoader gofer: ignored [

	| required |
	required := self resolveToLoadableSpec.
	required loader: aLoader.
	^required load
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> repository: aStringOrMetacelloRepositorySpec [
    ^ self projectReference repository: aStringOrMetacelloRepositorySpec
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveProjectSpec [

	^self projectReference
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToLoadableSpec [

	^self resolveProjectSpec resolveToLoadableSpec
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToPackagesAndProjectsIn: aVersionSpec visited: visited [

	^ { self }
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^#()
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> versionString [

	^self projectReference versionString
]
