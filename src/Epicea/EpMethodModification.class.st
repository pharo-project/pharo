"
I represent a modification of a method in a class or a trait.
"
Class {
	#name : #EpMethodModification,
	#superclass : #EpMethodChange,
	#instVars : [
		'oldMethod',
		'newMethod'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpMethodModification class >> oldMethod: anOldMethod newMethod: aNewMethod [

	^ self basicNew
		initializeWithOldMethod: anOldMethod
		newMethod: aNewMethod;
		yourself
]

{ #category : #visitor }
EpMethodModification >> accept: aVisitor [

	^ aVisitor visitMethodModification: self 
]

{ #category : #accessing }
EpMethodModification >> affectedPackageName [

	^ self newMethod package
]

{ #category : #initialization }
EpMethodModification >> initializeWithOldMethod: anOldMethod newMethod: aNewMethod [

	self initialize.
	oldMethod := anOldMethod asEpiceaRingDefinition.
	newMethod := aNewMethod asEpiceaRingDefinition.
]

{ #category : #accessing }
EpMethodModification >> methodAffected [
	
	^ newMethod
]

{ #category : #accessing }
EpMethodModification >> newMethod [

	^ newMethod
]

{ #category : #accessing }
EpMethodModification >> newProtocol [

	^ newMethod protocol
]

{ #category : #accessing }
EpMethodModification >> newSourceCode [

	^ newMethod sourceCode
]

{ #category : #accessing }
EpMethodModification >> oldMethod [

	^ oldMethod
]

{ #category : #accessing }
EpMethodModification >> oldProtocol [

	^ oldMethod protocol
]

{ #category : #accessing }
EpMethodModification >> oldSourceCode [

	^ oldMethod sourceCode
]
