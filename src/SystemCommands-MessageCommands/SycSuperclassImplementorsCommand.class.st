Class {
	#name : #SycSuperclassImplementorsCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSignature'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #execution }
SycSuperclassImplementorsCommand >> applyResultInContext: a [
]

{ #category : #accessing }
SycSuperclassImplementorsCommand >> defaultMenuIconName [
	^#edit
]

{ #category : #accessing }
SycSuperclassImplementorsCommand >> defaultMenuItemName [
	^'Superclass implementors'
]

{ #category : #execution }
SycSuperclassImplementorsCommand >> execute [

	| sup |
	sup := refactoredClass allSuperclasses 
		detect: [ :cl | cl includesSelector: originalMessage selector ] 
		ifNone: [ ^ self ].
	Smalltalk tools messageList browse: { sup >> originalMessage selector }
]

{ #category : #execution }
SycSuperclassImplementorsCommand >> isComplexRefactoring [

	^ false
]
