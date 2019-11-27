Class {
	#name : #DeprecatedGlobalVariable,
	#superclass : #GlobalVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #'code generation' }
DeprecatedGlobalVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self; send: #readDeprecatedValue
]
