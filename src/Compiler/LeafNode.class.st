"
I represent a leaf node of the compiler parse tree. I am abstract.
	
Types (defined in class ParseNode):
	1 LdInstType (which uses class VariableNode)
	2 LdTempType (which uses class VariableNode)
	3 LdLitType (which uses class LiteralNode)
	4 LdLitIndType (which uses class VariableNode)
	5 SendType (which uses class SelectorNode).

Note that Squeak departs slightly from the Blue Book bytecode spec.

In order to allow access to more than 63 literals and instance variables,
bytecode 132 has been redefined as DoubleExtendedDoAnything:
		byte2				byte3			Operation
(hi 3 bits)  (lo 5 bits)
	0		nargs			lit index			Send Literal Message 0-255
	1		nargs			lit index			Super-Send Lit Msg 0-255
	2		ignored			rcvr index		Push Receiver Variable 0-255
	3		ignored			lit index			Push Literal Constant 0-255
	4		ignored			lit index			Push Literal Variable 0-255
	5		ignored			rcvr index		Store Receiver Variable 0-255
	6		ignored			rcvr index		Store-pop Receiver Variable 0-255
	7		ignored			lit index			Store Literal Variable 0-255

	This has allowed bytecode 134 also to be redefined as a second extended send
	that can access literals up to 64 for nargs up to 3 without needing three bytes.
	It is just like 131, except that the extension byte is aallllll instead of aaalllll,
	where aaa are bits of argument count, and lll are bits of literal index.
"
Class {
	#name : #LeafNode,
	#superclass : #ParseNode,
	#instVars : [
		'key',
		'code',
		'index'
	],
	#category : #'Compiler-ParseNodes'
}

{ #category : #'code generation (closures)' }
LeafNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self
]

{ #category : #'code generation' }
LeafNode >> code [

	^ code
]

{ #category : #private }
LeafNode >> code: argIndex type: type [

	argIndex ifNil: [^type negated].
	(CodeLimits at: type) > argIndex 
		ifTrue: [^(CodeBases at: type) + argIndex].
	^type * 256 + argIndex
]

{ #category : #'code generation' }
LeafNode >> emitCodeForEffect: stack encoder: encoder [

	^self
]

{ #category : #'code generation' }
LeafNode >> emitCodeForLoad: stack encoder: encoder [
	"Default is to do nothing.
	 Subclasses may need to override."
]

{ #category : #accessing }
LeafNode >> key [

	^key
]

{ #category : #'initialize-release' }
LeafNode >> key: object code: byte [

	key := object.
	code := byte
]

{ #category : #'initialize-release' }
LeafNode >> key: object index: i type: type [

	key := object.
	code := (self code: i type: type).
	index := i
]

{ #category : #'initialize-release' }
LeafNode >> name: ignored key: object code: byte [

	key := object.
	code := byte
]

{ #category : #'code generation' }
LeafNode >> reserve: encoder [ 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder litIndex: key) type: 0 - code]
]

{ #category : #'code generation' }
LeafNode >> sizeCodeForEffect: encoder [

	^0
]

{ #category : #'code generation' }
LeafNode >> sizeCodeForLoad: encoder [
	"Default is to do nothing.
	 Subclasses may need to override."
	^0
]

{ #category : #'code generation' }
LeafNode >> sizeCodeForValue: encoder [
	self subclassResponsibility
]

{ #category : #copying }
LeafNode >> veryDeepFixupWith: deepCopier [
	"If fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
key := (deepCopier references at: key ifAbsent: [key]).

]

{ #category : #copying }
LeafNode >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"key := key.		Weakly copied"
code := code veryDeepCopyWith: deepCopier.
index := index veryDeepCopyWith: deepCopier.

]
