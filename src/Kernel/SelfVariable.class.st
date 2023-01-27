"
I model ""self"" keyword 
"
Class {
	#name : #SelfVariable,
	#superclass : #ReservedVariable,
	#category : #'Kernel-Variables'
}

{ #category : #accessing }
SelfVariable class >> variableName [
	^#self
]

{ #category : #visiting }
SelfVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [

	^ aProgramNodeVisitor visitSelfNode: aNode
]

{ #category : #emitting }
SelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : #testing }
SelfVariable >> isSelfVariable [
	^true
]

{ #category : #debugging }
SelfVariable >> readInContext: aContext [
	^aContext receiver
]

{ #category : #queries }
SelfVariable >> usingMethods [
	"Super sends are doing a pushSelf, too, we need to still check the AST level sometimes.
	FFI methods after a first call do not have a self send in the bytecode, but one in the code"

	^ environment allMethods select: [ :method |
		method isFFIMethod or: [
		  method readsSelf and: [
			  method sendsToSuper not or: [
				  method ast variableNodes anySatisfy: [ :varNode |
					  varNode variable == self ] ] ] ] ]
]
