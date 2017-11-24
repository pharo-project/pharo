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
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'jf.configcleanup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup.extraspeedup.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr.configcleanup.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 69.

	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr.configcleanup42.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 69

]

{ #category : #'tests - reference' }
GoferReferenceTest >> testContraintShouldFindLatestVersion [
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
]

{ #category : #'tests - working' }
GoferReferenceTest >> testContraintShouldFindWorkingCopy [
	| constraintReference workingCopy |
	constraintReference := GoferConstraintReference 
		name: 'Gofer-Core'
		constraint: [ :reference | false ].
	workingCopy := constraintReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'
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
				ifTrue: [ self assert: second <= first ] ] ]
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testPackageShouldFindLatestVersion [
	| packageReference reference |
	packageReference := GoferPackageReference name: 'GoferFoo'.
	reference := packageReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 4.
	self assert: reference repository = self monticelloRepository
]

{ #category : #'tests - working' }
GoferReferenceTest >> testPackageShouldFindWorkingCopy [
	| packageReference workingCopy |
	packageReference := GoferPackageReference name: 'Gofer-Core'.
	workingCopy := packageReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testResolvedShouldFindLatestVersion [
	| versionReference reference |
	versionReference := GoferResolvedReference name: 'GoferFoo-lr.2' repository: self monticelloRepository.
	reference := versionReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 2.
	self assert: reference repository = self monticelloRepository
]

{ #category : #'tests - working' }
GoferReferenceTest >> testResolvedShouldFindWorkingCopy [
	| versionReference workingCopy |
	versionReference := GoferResolvedReference name: 'Gofer-Core-lr.18' repository: self monticelloRepository.
	workingCopy := versionReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'
]

{ #category : #'tests - reference' }
GoferReferenceTest >> testVersionShouldFindLatestVersion [
	| versionReference reference |
	versionReference := GoferVersionReference name: 'GoferFoo-lr.2'.
	reference := versionReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference versionNumber = 2.
	self assert: reference branch isEmpty.
	self assert: reference repository = self monticelloRepository.
	
	versionReference := GoferVersionReference name: 'GoferFoo-lr.3'.
	self should: [ versionReference resolveWith: gofer ] raise: Error
]

{ #category : #'tests - working' }
GoferReferenceTest >> testVersionShouldFindWorkingCopy [
	| versionReference workingCopy |
	versionReference := GoferVersionReference name: 'Gofer-Core-lr.18'.
	workingCopy := versionReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'
]

{ #category : #tests }
GoferReferenceTest >> testVersionShouldParseComplexName [
	| queryReference |
	queryReference := GoferVersionReference name: 'Seaside2.8b5'.
	self assert: queryReference packageName = 'Seaside2'.
	self assert: queryReference author isEmpty.
	self assert: queryReference branch = '8b5'.
	self assert: queryReference versionNumber = 0.
	
	queryReference := GoferVersionReference name: 'Seaside2.8b5-avi.1'.
	self assert: queryReference packageName = 'Seaside2'.
	self assert: queryReference author = 'avi'.
	self assert: queryReference branch = '8b5'.
	self assert: queryReference versionNumber = 1.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-pmm.2'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'pmm'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 2.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup-jf.3'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'jf'.
	self assert: queryReference branch = 'configcleanup'.
	self assert: queryReference versionNumber = 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup.extraspeedup-lr.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr'.
	self assert: queryReference branch = 'configcleanup.extraspeedup'.
	self assert: queryReference versionNumber = 69.

	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr'.
	self assert: queryReference branch = 'configcleanup42.extraspeedup'.
	self assert: queryReference versionNumber = 69

]
