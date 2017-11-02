"
I represent a addition of a method in a class or a trait.
"
Class {
	#name : #EpMethodAddition,
	#superclass : #EpMethodChange,
	#instVars : [
		'method'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpMethodAddition class >> method: aCompiledMethod [
	
	^ self basicNew
		initializeMethod: aCompiledMethod;
		yourself
]

{ #category : #visitor }
EpMethodAddition >> accept: aVisitor [

	^ aVisitor visitMethodAddition: self
]

{ #category : #accessing }
EpMethodAddition >> affectedPackageName [

	^ method package
]

{ #category : #initialization }
EpMethodAddition >> initializeMethod: aCompiledMethod [

	self initialize.
	method := aCompiledMethod asEpiceaRingDefinition.
]

{ #category : #accessing }
EpMethodAddition >> methodAdded [

	^ method
]

{ #category : #accessing }
EpMethodAddition >> methodAffected [

	^ self methodAdded
]

{ #category : #accessing }
EpMethodAddition >> protocol [

	^ method protocol
]

{ #category : #accessing }
EpMethodAddition >> selector [

	^ method selector
]

{ #category : #accessing }
EpMethodAddition >> sourceCode [

	^ method sourceCode
]
