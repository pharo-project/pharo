"
When handing over a dictionary with additonal binding to the compiler:

	 Smalltalk compiler
        bindings: {(#test -> Point)} asDictionary;
        evaluate: 'test := 42'.

all associations are changed to be AdditionaBinding.
"
Class {
	#name : #AdditionalBinding,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #'code generation' }
AdditionalBinding >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
AdditionalBinding >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self.
]
