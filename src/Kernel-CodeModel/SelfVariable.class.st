"
I model ""self""
"
Class {
	#name : 'SelfVariable',
	#superclass : 'PseudoVariable',
	#category : 'Kernel-CodeModel-Variables',
	#package : 'Kernel-CodeModel',
	#tag : 'Variables'
}

{ #category : 'accessing' }
SelfVariable class >> variableName [
	^#self
]

{ #category : 'visiting' }
SelfVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [

	^ aProgramNodeVisitor visitSelfNode: aNode
]

{ #category : 'code generation' }
SelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : 'testing' }
SelfVariable >> isSelfVariable [
	^true
]

{ #category : 'debugging' }
SelfVariable >> readInContext: aContext [
	"self in a block is the receiver of the home context
	For clean blocks it might not be known (nil)"
	^aContext home ifNotNil: [:home | home receiver ]
]

{ #category : 'queries' }
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
