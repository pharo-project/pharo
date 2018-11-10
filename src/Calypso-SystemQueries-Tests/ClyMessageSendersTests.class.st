Class {
	#name : #ClyMessageSendersTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyMessageSendersTests class >> classSideSender1OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersTests class >> classSideSender2OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #running }
ClyMessageSendersTests >> createQuery [
	^ ClyMessageSenders
		of: self referencedSelector
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #running }
ClyMessageSendersTests >> referencedSelector [
	"String is used here to not make this method sender of it"
	^'clyReferencedMessage1' asSymbol
]

{ #category : #'methods for tests' }
ClyMessageSendersTests >> sender1OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersTests >> sender2OfMessage1 [
	self clyReferencedMessage1
]

{ #category : #'methods for tests' }
ClyMessageSendersTests >> senderOfIfTrue [
	^2*2 = 4 ifTrue: [100]
]

{ #category : #'methods for tests' }
ClyMessageSendersTests >> senderOfMessage2 [
	self message2
]

{ #category : #tests }
ClyMessageSendersTests >> testCheckIfEmpty [
	query scope:	(ClyMethodScope of: (self class >> #sender1OfMessage1) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (self class >> #senderOfMessage2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMessageSendersTests >> testComparison [

	self 
		assert: (ClyMessageSenders of: #message1) 
		equals: (ClyMessageSenders of: #message1).
	
	self 
		assert: (ClyMessageSenders of: #message1) hash 
		equals: (ClyMessageSenders of: #message1) hash.
		
	self deny: (ClyMessageSenders of: #message1) = (ClyMessageSenders of: #message2)
]

{ #category : #tests }
ClyMessageSendersTests >> testDetectingSpecialSelectorLikeIfTrue [

	query := ClyMessageSenders of: ##ifTrue:.
	
	self queryFromScope: ClyMethodScope of: self class >> #senderOfIfTrue.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #senderOfIfTrue)
]

{ #category : #tests }
ClyMessageSendersTests >> testFromLocalClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1)
	
]

{ #category : #tests }
ClyMessageSendersTests >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #sender1OfMessage1. self class >> #senderOfMessage2 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #sender1OfMessage1)
]

{ #category : #tests }
ClyMessageSendersTests >> testFromPackageScope [
	
	self queryFromScope: ClyPackageScope of: self class package.	
		
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1 
		superclassSenderOfMessage1)
]

{ #category : #tests }
ClyMessageSendersTests >> testFromSystemScope [
	
	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.	
	
	self assert: foundSelectors equals: #(classSideSender1OfMessage1 classSideSender2OfMessage1 
		sender1OfMessage1 sender2OfMessage1 
		superclassSenderOfMessage1)
]
