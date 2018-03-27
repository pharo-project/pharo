"
SUnit tests for GoferReference and subclasses
"
Class {
	#name : #GoferReferenceTest,
	#superclass : #GoferTest,
	#category : #'Gofer-Tests-Tests'
}

{ #category : #running }
GoferReferenceTest >> setUp [
	super setUp.
	gofer repository: self monticelloRepository
]

{ #category : #tests }
GoferReferenceTest >> testBranchAfterAuthorIsNotABranch [
	| queryReference |
	queryReference := GoferVersionReference name: 'Seaside-Core-jf.configcleanup.3'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'jf.configcleanup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber equals: 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup.extraspeedup.69'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'lr.configcleanup.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber equals: 69.

	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'lr.configcleanup42.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber equals: 69

]

{ #category : #'tests - reference' }
GoferReferenceTest >> testContraintShouldFindLatestVersion [
	| constraintReference reference |
	constraintReference := GoferConstraintReference 
		name: 'GoferBar'
		constraint: [ :ref | true ].
	self assert: (constraintReference resolveAllWith: gofer) size equals: 4.
	
	reference := constraintReference resolveWith: gofer.
		
	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 1.
	self assert: reference repository equals: self monticelloRepository.
	
	constraintReference := GoferConstraintReference 
									name: 'GoferBar'
									constraint: [ :ref | ref branch = 'branch' ].

	self assert: (constraintReference resolveAllWith: gofer) size equals: 2.

	reference := constraintReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch equals: 'branch'.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository.
	
	constraintReference := GoferConstraintReference 
									name: 'GoferBar'
									constraint: [ :ref | ref author = 'jf' ].

	self assert: (constraintReference resolveAllWith: gofer) size equals: 1.

	reference := constraintReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'jf'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 1.
	self assert: reference repository equals: self monticelloRepository.
	
	constraintReference := GoferConstraintReference 
										name: 'GoferBar'
										constraint: [ :ref | false ].
										
	self assert: (constraintReference resolveAllWith: gofer) isEmpty.
	self should: [ constraintReference resolveWith: gofer ] raise: Error.
]

{ #category : #'tests - working' }
GoferReferenceTest >> testContraintShouldFindWorkingCopy [
	| constraintReference workingCopy |
	constraintReference := GoferConstraintReference 
		name: 'Gofer-Core'
		constraint: [ :reference | false ].
	workingCopy := constraintReference workingCopy.
	self assert: workingCopy packageName equals: 'Gofer-Core'
]

{ #category : #tests }
GoferReferenceTest >> testLoadableShouldSortCorrectly [
	| sorted |
	sorted := self versionReferences
		collect: [ :each | each resolveWith: gofer ].
	sorted withIndexDo: [ :first :firstIndex |
		sorted withIndexDo: [ :second :secondIndex |
			firstIndex <= secondIndex
				ifTrue: [ self assert: first <= second ].
			firstIndex >= secondIndex
				ifTrue: [ self assert: second <= first ]]]
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testPackageShouldFindLatestVersion [
	| packageReference reference |
	packageReference := GoferPackageReference name: 'GoferFoo'.
	reference := packageReference resolveWith: gofer.

	self assert: reference packageName equals: 'GoferFoo'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 4.
	self assert: reference repository equals: self monticelloRepository
]

{ #category : #'tests - working' }
GoferReferenceTest >> testPackageShouldFindWorkingCopy [
	| packageReference workingCopy |
	packageReference := GoferPackageReference name: 'Gofer-Core'.
	workingCopy := packageReference workingCopy.
	self assert: workingCopy packageName equals: 'Gofer-Core'
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testResolvedShouldFindLatestVersion [
	| versionReference reference |
	versionReference := GoferResolvedReference name: 'GoferFoo-lr.2' repository: self monticelloRepository.
	reference := versionReference resolveWith: gofer.
	
	self assert: reference packageName equals: 'GoferFoo'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository
]

{ #category : #'tests - working' }
GoferReferenceTest >> testResolvedShouldFindWorkingCopy [
	| versionReference workingCopy |
	versionReference := GoferResolvedReference name: 'Gofer-Core-lr.18' repository: self monticelloRepository.
	workingCopy := versionReference workingCopy.
	self assert: workingCopy packageName equals: 'Gofer-Core'
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testVersionShouldFindLatestVersion [
	| versionReference reference |
	versionReference := GoferVersionReference name: 'GoferFoo-lr.2'.
	reference := versionReference resolveWith: gofer.
	
	self assert: reference packageName equals: 'GoferFoo'.
	self assert: reference author equals: 'lr'.
	self assert: reference versionNumber equals: 2.
	self assert: reference branch isEmpty.
	self assert: reference repository equals: self monticelloRepository.
	
	versionReference := GoferVersionReference name: 'GoferFoo-lr.3'.
	self should: [ versionReference resolveWith: gofer ] raise: Error
]

{ #category : #'tests - working' }
GoferReferenceTest >> testVersionShouldFindWorkingCopy [
	| versionReference workingCopy |
	versionReference := GoferVersionReference name: 'Gofer-Core-lr.18'.
	workingCopy := versionReference workingCopy.
	
	self assert: workingCopy packageName equals: 'Gofer-Core'
]

{ #category : #tests }
GoferReferenceTest >> testVersionShouldParseComplexName [
	| queryReference |
	
	queryReference := GoferVersionReference name: 'Seaside2.8b5'.
	self assert: queryReference packageName equals: 'Seaside2'.
	self assert: queryReference author isEmpty.
	self assert: queryReference branch equals: '8b5'.
	self assert: queryReference versionNumber equals: 0.
	
	queryReference := GoferVersionReference name: 'Seaside2.8b5-avi.1'.
	self assert: queryReference packageName equals: 'Seaside2'.
	self assert: queryReference author equals: 'avi'.
	self assert: queryReference branch equals: '8b5'.
	self assert: queryReference versionNumber equals: 1.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-pmm.2'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'pmm'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber equals: 2.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup-jf.3'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'jf'.
	self assert: queryReference branch equals: 'configcleanup'.
	self assert: queryReference versionNumber equals: 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup.extraspeedup-lr.69'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'lr'.
	self assert: queryReference branch equals: 'configcleanup.extraspeedup'.
	self assert: queryReference versionNumber equals: 69.

	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'.
	self assert: queryReference packageName equals: 'Seaside-Core'.
	self assert: queryReference author equals: 'lr'.
	self assert: queryReference branch equals: 'configcleanup42.extraspeedup'.
	self assert: queryReference versionNumber equals: 69

]
