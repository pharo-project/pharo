"
I am a dumb visitor for sindarin command tests
"
Class {
	#name : #SindarinTestVisitor,
	#superclass : #Object,
	#category : #'NewTools-Sindarin-Commands-Tests'
}

{ #category : #visiting }
SindarinTestVisitor >> doNotVisit: anObject [
	anObject doNotAcceptVisitor: self
]

{ #category : #visiting }
SindarinTestVisitor >> visit: anObject [
	anObject acceptVisitor: self
]

{ #category : #visiting }
SindarinTestVisitor >> visitMultipleObjects: anObject [
	
	anObject acceptVisitorMultipleObjects: self
]

{ #category : #visiting }
SindarinTestVisitor >> visitNeverFinish: anObject [
	anObject acceptVisitorNeverFinishes: self
]

{ #category : #visiting }
SindarinTestVisitor >> visitTestObject: anObject [

	^self
]

{ #category : #visiting }
SindarinTestVisitor >> visitWithError: anObject [
	anObject acceptVisitorWithError: self
]
