"
I represent a removal of a method in a class or a trait.
"
Class {
	#name : #EpMethodRemoval,
	#superclass : #EpMethodChange,
	#instVars : [
		'method'
	],
	#category : #Epicea-Model
}

{ #category : #'instance creation' }
EpMethodRemoval class >> method: aCompiledMethod [

	^ self basicNew
		initializeWithMethod: aCompiledMethod;
		yourself
]

{ #category : #visitor }
EpMethodRemoval >> accept: aVisitor [

	^ aVisitor visitMethodRemoval: self 
]

{ #category : #accessing }
EpMethodRemoval >> affectedPackageName [

	^ method package
]

{ #category : #initialization }
EpMethodRemoval >> initializeWithMethod: aCompiledMethod [

	self initialize.
	method := aCompiledMethod asEpiceaRingDefinition.
]

{ #category : #accessing }
EpMethodRemoval >> methodAffected [

	^ method
]

{ #category : #accessing }
EpMethodRemoval >> methodRemoved [

	^ method
]

{ #category : #accessing }
EpMethodRemoval >> protocol [

	^ method protocol
]

{ #category : #accessing }
EpMethodRemoval >> selector [

	^ method selector
]

{ #category : #accessing }
EpMethodRemoval >> sourceCode [

	^ method sourceCode
]
