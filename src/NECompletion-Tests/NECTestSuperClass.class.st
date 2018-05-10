"
I'm only for SUnit TestCases.
"
Class {
	#name : #NECTestSuperClass,
	#superclass : #Object,
	#instVars : [
		'superInstVar'
	],
	#classVars : [
		'SuperClassVar'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #initialization }
NECTestSuperClass class >> initialize [
	SuperClassVar := Dictionary new
]

{ #category : #initialization }
NECTestSuperClass >> initialize [
	superInstVar := Dictionary new
]

{ #category : #tests }
NECTestSuperClass >> testIt: aString [
	self subclassResponsibility 
]

{ #category : #utilities }
NECTestSuperClass >> toBeOverriden: anArgument [ 
	15 > 16
		ifTrue: [self sample * anArgument ]
]

{ #category : #utilities }
NECTestSuperClass >> toBeOverridenWithReturn [
	^ 'saga'
]
