Class {
	#name : 'ClyClassExternalReferencesQueryTest',
	#superclass : 'ClyMethodQueryTest',
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'running' }
ClyClassExternalReferencesQueryTest >> createQuery [
	^ ClyClassExternalReferencesQuery
		to: self referencedClass
		from: (ClyClassScope of: Object in: environment)
]

{ #category : 'running' }
ClyClassExternalReferencesQueryTest >> referencedClass [
	"Use an explicit reference instead of 'self class'"
	^ClyClassExternalReferencesQueryTest
]

{ #category : 'tests' }
ClyClassExternalReferencesQueryTest >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	"Expect the query to be empty even though the class references itself in #referencedClass"
	self assert: resultItems isEmpty
]
