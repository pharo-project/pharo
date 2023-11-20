Extension { #name : 'GoferReferenceTest' }

{ #category : '*Gofer-Deprecated' }
GoferReferenceTest >> testContraintShouldFindLatestVersion [

	| constraintReference reference |
	constraintReference := GoferConstraintReference name: 'GoferBar' constraint: [ :ref | true ].
	self assert: (constraintReference resolveAllWith: gofer) size equals: 4.

	reference := constraintReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assertEmpty: reference branch.
	self assert: reference versionNumber equals: 1.
	self assert: reference repository equals: self monticelloRepository.

	constraintReference := GoferConstraintReference name: 'GoferBar' constraint: [ :ref | ref branch = 'branch' ].

	self assert: (constraintReference resolveAllWith: gofer) size equals: 2.

	reference := constraintReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch equals: 'branch'.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository.

	constraintReference := GoferConstraintReference name: 'GoferBar' constraint: [ :ref | ref author = 'jf' ].

	self assert: (constraintReference resolveAllWith: gofer) size equals: 1.

	reference := constraintReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'jf'.
	self assertEmpty: reference branch.
	self assert: reference versionNumber equals: 1.
	self assert: reference repository equals: self monticelloRepository.

	constraintReference := GoferConstraintReference name: 'GoferBar' constraint: [ :ref | false ].

	self assertEmpty: (constraintReference resolveAllWith: gofer).
	self should: [ constraintReference resolveWith: gofer ] raise: Error
]

{ #category : '*Gofer-Deprecated' }
GoferReferenceTest >> testContraintShouldFindWorkingCopy [

	| constraintReference workingCopy |
	constraintReference := GoferConstraintReference
		name: 'Gofer-Core'
		constraint: [ :reference | false ].
	workingCopy := constraintReference workingCopy.
	self assert: workingCopy packageName equals: 'Gofer-Core'
]
