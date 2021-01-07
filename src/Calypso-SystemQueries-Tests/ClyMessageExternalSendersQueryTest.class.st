Class {
	#name : #ClyMessageExternalSendersQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyMessageExternalSendersQueryTest >> createQuery [
	^ ClyMessageExternalSendersQuery
		of: self referencedSelector
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #running }
ClyMessageExternalSendersQueryTest >> referencedSelector [
	^#referencedSelector
]

{ #category : #tests }
ClyMessageExternalSendersQueryTest >> testFromSystemScope [
	
	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.	
	
	self assert: foundSelectors isEmpty
]
