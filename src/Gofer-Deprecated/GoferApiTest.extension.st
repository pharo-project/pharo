Extension { #name : 'GoferApiTest' }

{ #category : '*Gofer-Deprecated' }
GoferApiTest >> testConstraintReference [

	| reference |
	gofer
		repository: self monticelloRepository;
		package: 'GoferBar' constraint: [ :ref | ref branch = 'branch' ].

	self assert: gofer resolved size equals: 1.

	reference := gofer resolved first.

	self assert: (reference isKindOf: GoferResolvedReference).
	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch equals: 'branch'.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository
]
