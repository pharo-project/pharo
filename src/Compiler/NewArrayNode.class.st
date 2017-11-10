"
I represent a node for the genPushNewArray: opcode.
"
Class {
	#name : #NewArrayNode,
	#superclass : #ParseNode,
	#instVars : [
		'numElements'
	],
	#category : #Compiler-ParseNodes
}

{ #category : #visiting }
NewArrayNode >> accept: aVisitor [
	^aVisitor visitNewArrayNode: self
]

{ #category : #'code generation (closures)' }
NewArrayNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self
]

{ #category : #'code generation' }
NewArrayNode >> emitCodeForValue: stack encoder: encoder [
	encoder genPushNewArray: numElements.
	stack push: 1
]

{ #category : #accessing }
NewArrayNode >> numElements [
	^numElements
]

{ #category : #accessing }
NewArrayNode >> numElements: n [
	numElements := n
]

{ #category : #'code generation' }
NewArrayNode >> sizeCodeForValue: encoder [
	^encoder sizePushNewArray: numElements
]
