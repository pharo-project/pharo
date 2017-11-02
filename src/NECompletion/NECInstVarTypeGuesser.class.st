"
I'm a simple InstructionClient that tries to guess the type of a given instance variable name of a class. 
"
Class {
	#name : #NECInstVarTypeGuesser,
	#superclass : #NECVarTypeGuesser,
	#instVars : [
		'varIndex'
	],
	#category : 'NECompletion-Model'
}

{ #category : #public }
NECInstVarTypeGuesser >> methodRefs [
	| theClass selectors |
	theClass := receiverClass classThatDefinesInstanceVariable: variableName.
	theClass ifNil: [ ^ nil ].
	selectors := theClass whichSelectorsStoreInto: variableName.
	^ selectors collect: [ :each | 
		RGMethodDefinition realClass: theClass selector: each]
]

{ #category : #'bytecode decoding' }
NECInstVarTypeGuesser >> popIntoReceiverVariable: offset [ 
	varIndex = offset 
		ifTrue: [ found := true ]
		ifFalse: [ self reset ]
]

{ #category : #'bytecode decoding' }
NECInstVarTypeGuesser >> prepare: aCompiledMethod [ 
	| theClass |
	super prepare: aCompiledMethod.
	theClass := aCompiledMethod realClass.
	varIndex := (theClass allInstVarNames indexOf: variableName) - 1
]
