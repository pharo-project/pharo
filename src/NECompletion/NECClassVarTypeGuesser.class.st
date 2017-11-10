"
I'm a simple InstructionClient that tries to guess the type of a given class variable name of a class. 
"
Class {
	#name : #NECClassVarTypeGuesser,
	#superclass : #NECVarTypeGuesser,
	#category : #NECompletion-Model
}

{ #category : #public }
NECClassVarTypeGuesser >> methodRefs [
	| theClass classVarAssoc |
	theClass := receiverClass classThatDefinesClassVariable: variableName.
	classVarAssoc := theClass classPool associationAt: variableName asSymbol.
	classVarAssoc value 
		ifNil: [ ^ SystemNavigation new allCallsOn: classVarAssoc ]
		ifNotNil: [ ^ classVarAssoc value class ]
]

{ #category : #'bytecode decoding' }
NECClassVarTypeGuesser >> popIntoLiteralVariable: anAssociation [ 
	anAssociation key == variableName asSymbol 
		ifTrue: [ found := true ]
		ifFalse: [ self reset ]
]

{ #category : #'instruction decoding' }
NECClassVarTypeGuesser >> popIntoReceiverVariable: offset [ 
	self reset
]

{ #category : #'bytecode decoding' }
NECClassVarTypeGuesser >> send: selector super: supered numArgs: numberArguments [ 

]
