Class {
	#name : #ClyMessageSendersQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest class >> classSideSender1OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest class >> classSideSender2OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #running }
ClyMessageSendersQueryTest >> createQuery [
	^ ClyMessageSendersQuery
		of: self referencedSelector
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #running }
ClyMessageSendersQueryTest >> referencedSelector [
	"String is used here to not make this method sender of it"
	^'clyReferencedMessage1' asSymbol
]

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest >> sender1OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest >> sender2OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest >> senderOfIfTrue [
	^2*2 = 4 ifTrue: [100]
]

{ #category : #'methods for tests' }
ClyMessageSendersQueryTest >> senderOfMessage2 [
	self message2
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testCheckIfEmpty [
	query scope:	(ClyMethodScope of: (self class >> #sender1OfMessage1) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (self class >> #senderOfMessage2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testComparison [

	self 
		assert: (ClyMessageSendersQuery of: #message1) 
		equals: (ClyMessageSendersQuery of: #message1).
	
	self 
		assert: (ClyMessageSendersQuery of: #message1) hash 
		equals: (ClyMessageSendersQuery of: #message1) hash.
		
	self deny: (ClyMessageSendersQuery of: #message1) = (ClyMessageSendersQuery of: #message2)
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testDetectingSpecialSelectorLikeIfTrue [

	query := ClyMessageSendersQuery of: ##ifTrue:.
	
	self queryFromScope: ClyMethodScope of: self class >> #senderOfIfTrue.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #senderOfIfTrue)
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testFromLocalClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1)
	
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #sender1OfMessage1. self class >> #senderOfMessage2 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #sender1OfMessage1)
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testFromPackageScope [
	
	self queryFromScope: ClyPackageScope of: self class package.	
		
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1 
		superclassSenderOfMessage1)
]

{ #category : #tests }
ClyMessageSendersQueryTest >> testFromSystemScope [
	
	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.	
	
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1 
		superclassSenderOfMessage1)
]
