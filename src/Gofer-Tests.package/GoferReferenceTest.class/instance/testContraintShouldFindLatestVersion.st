tests - reference
testContraintShouldFindLatestVersion
	| constraintReference reference |
	constraintReference := GoferConstraintReference 
		name: 'GoferBar'
		constraint: [ :ref | true ].
	self assert: (constraintReference resolveAllWith: gofer) size = 4.
	reference := constraintReference resolveWith: gofer.
	self assert: reference packageName = 'GoferBar'.
	self assert: reference author = 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 1.
	self assert: reference repository = self monticelloRepository.
	
	constraintReference := GoferConstraintReference 
		name: 'GoferBar'
		constraint: [ :ref | ref branch = 'branch' ].
	self assert: (constraintReference resolveAllWith: gofer) size = 2.
	reference := constraintReference resolveWith: gofer.
	self assert: reference packageName = 'GoferBar'.
	self assert: reference author = 'lr'.
	self assert: reference branch = 'branch'.
	self assert: reference versionNumber = 2.
	self assert: reference repository = self monticelloRepository.
	
	constraintReference  := GoferConstraintReference 
		name: 'GoferBar'
		constraint: [ :ref | ref author = 'jf' ].
	self assert: (constraintReference resolveAllWith: gofer) size = 1.
	reference := constraintReference resolveWith: gofer.
	self assert: reference packageName = 'GoferBar'.
	self assert: reference author = 'jf'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 1.
	self assert: reference repository = self monticelloRepository.
	
	constraintReference  := GoferConstraintReference 
		name: 'GoferBar'
		constraint: [ :ref | false ].
	self assert: (constraintReference resolveAllWith: gofer) isEmpty.
	self should: [ constraintReference resolveWith: gofer ] raise: Error.