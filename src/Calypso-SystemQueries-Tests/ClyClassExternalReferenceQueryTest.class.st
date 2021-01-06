Class {
	#name : #ClyClassExternalReferenceQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyClassExternalReferenceQueryTest >> createQuery [
	^ ClyClassExternalReferencesQuery
		to: self referencedClass
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #running }
ClyClassExternalReferenceQueryTest >> referencedClass [
	"Use an explicit reference instead of 'self class'"
	^ClyClassExternalReferenceQueryTest
]

{ #category : #tests }
ClyClassExternalReferenceQueryTest >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	"Expect the query to be empty even though the class references itself in #referencedClass"
	self assert: resultItems isEmpty
]
