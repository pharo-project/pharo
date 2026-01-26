Class {
	#name : 'ReClassToBeDuplicatedWithTraits',
	#superclass : 'Object',
	#traits : 'TReTraitToBeUsedInDuplication',
	#classTraits : 'TReTraitToBeUsedInDuplication classTrait',
	#instVars : [
		'instVar1'
	],
	#category : 'Refactoring-DataForTesting-ForDuplication',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForDuplication'
}

{ #category : 'comparing' }
ReClassToBeDuplicatedWithTraits >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ instVar1 = anObject instVar1
]

{ #category : 'comparing' }
ReClassToBeDuplicatedWithTraits >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ instVar1 hash
]
