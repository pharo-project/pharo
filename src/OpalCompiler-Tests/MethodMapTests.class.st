Class {
	#name : #MethodMapTests,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #util }
MethodMapTests >> compileAndRunExample: aSelector [
	| cm |
	
	cm := self compileMethod:  MethodMapExamples>>aSelector.
	^cm valueWithReceiver:  MethodMapExamples new arguments: #().
]

{ #category : #util }
MethodMapTests >> compileMethod: aMethod [

	^aMethod parseTree generate: aMethod trailer.
	
]

{ #category : #'testing - ast mapping' }
MethodMapTests >> deadContext [
	^ thisContext
]

{ #category : #'testing - ast mapping' }
MethodMapTests >> inlinedBlockSourceNode [
	1 to: 1 do: [ :i | ^ thisContext sourceNode ]. 


]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testBlockAndContextSourceNode [
		
	|block blockNodeViaContext blockNodeViaClosure |


	block := [blockNodeViaContext := thisContext sourceNode].
	block value.
	blockNodeViaClosure := block sourceNode.

	self assert: blockNodeViaContext == blockNodeViaClosure
	


]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testBlockSourceNode [
	| sourceNode |
	sourceNode := [ 1 + 2 ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ 1 + 2 ]').


]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testBlockWithArgAndEnclosedBlockSourceNode [
	| sourceNode |
	sourceNode := [ :arg |  [ arg ] ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ :arg | [ arg ] ]').

]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testBlockWithEnclosedBlockSourceNode [
	| sourceNode |
	sourceNode := [ [ ] ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ [ ] ]').

]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testBlockWithTempsSourceNode [
	| sourceNode |
	sourceNode := [ | t1 t2 | ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ | t1 t2 | ]').

]

{ #category : #'testing - temp access' }
MethodMapTests >> testCopiedVarFromDeadContext [
	self assert:  (self compileAndRunExample:  #exampleCopiedVarFromDeadContext) equals: 2.
]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testDeadContextSourceNode [
	| deadContext |
	deadContext := self deadContext.
	self assert: deadContext isDead. 
	self assert: deadContext sourceNode equals: (self class>>#deadContext) ast

]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleSimpleTemp [
	self assert: (self compileAndRunExample: #exampleSimpleTemp) equals: 1
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedCopying [
	self assert: (self compileAndRunExample: #exampleTempNamedCopying) equals: 1
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedCopying2 [
	self assert: (self compileAndRunExample: #exampleTempNamedCopying2) equals: 1
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedPutCopying [
	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying) equals: 2.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedPutCopying2 [
"modifying a copied temp variable will *not* modify the value in the outer context. 
(See other test case OCClosureCompilerTest>>#testDebuggerTempAccess and notes on fogbugz issue 17702"
	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying2) equals: 1
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedPutTempVector [
	self assert: (self compileAndRunExample: #exampleTempNamedPutTempVector) equals: 3.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedPutTempVector2 [
	self assert: (self compileAndRunExample: #exampleTempNamedPutTempVector2) equals: 3
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedTempVector [
	self assert: (self compileAndRunExample: #exampleTempNamedTempVector) equals: 2
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedTempVector2 [
	self assert: (self compileAndRunExample: #exampleTempNamedTempVector2) equals: 2
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedTempVectorInOptimizedBlock [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorInOptimizedBlock ) equals: 2.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedTempVectorInlinedLoop [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorInlinedLoop ) equals: 42.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testExampleTempNamedTempVectorNestedBlock [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorNestedBlock ) equals: 2.
]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testMethodSourceNodeAtInitialPC [

	| method actual |
	method := self class >> testSelector.
	actual := method sourceNodeForPC: method initialPC.
	
	self assert: actual equals: method ast sendNodes first receiver receiver

]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testMethodSourceNodeAtPC [
	self assert: (((Object>>#halt) sourceNodeForPC:  (Smalltalk vm for32bit: 22 for64bit: 42)) isKindOf: RBMessageNode).

]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testPrimitiveMethodSourceNodeAtInitialPC [

	| method actual |
	method := SmallInteger >> #+.
	actual := method sourceNodeForPC: method initialPC.
	
	self assert: actual equals: method ast

]

{ #category : #'testing - source mapping' }
MethodMapTests >> testSimpleSourceMapping [
	| method range highlight |

	method := Object>>('ha', 'lt') asSymbol.

	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: (Smalltalk vm for32bit: 23 for64bit: 43).
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'now'.


	
]

{ #category : #'testing - source mapping' }
MethodMapTests >> testSourceMappingBlock [
	| method range highlight pcs |

	method := MethodMapExamples>>#exampleTempNamedCopying.
	
	"Cannot check for class since those encoders are changing names all the time..."
	pcs := (Smalltalk vm 
		for32bit: [
			(method encoderClass name endsWith: 'SistaV1')
				ifTrue: [ 41 to: 45 ]
				ifFalse: [ 42 to: 46 ] ] 
		for64bit: [
			(method encoderClass name endsWith: 'SistaV1')
				ifTrue: [ 73 to: 77 ]
				ifFalse: [ 74 to: 78 ] ]).		
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: pcs first.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'b'.

	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: pcs second.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'a := b'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: pcs third.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals:'DebuggerMethodMapOpal'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: pcs fourth.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals:'thisContext'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: pcs fifth.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'method'.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testTempNamedTempCopyingNestedBlock [
	self assert: (self compileAndRunExample:  #exampleTempNamedTempCopyingNestedBlock) equals: 1.
]

{ #category : #'testing - temp access' }
MethodMapTests >> testTempNamedTempCopyingNestedBlockPROBLEM [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempCopyingNestedBlockPROBLEM) equals: 1.
]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testThisContextSourceNode [
	self assert: (thisContext sourceNode isKindOf: RBMethodNode).
	self assert: ([thisContext sourceNode] value isKindOf: RBBlockNode).
	self assert: ([true ifTrue: [thisContext sourceNode]]value isKindOf: RBBlockNode).



]

{ #category : #'testing - ast mapping' }
MethodMapTests >> testThisContextSourceNodeInInlinedMessage [
	| inlinedBlockSourceNode |
	inlinedBlockSourceNode := self inlinedBlockSourceNode.
	self assert: (inlinedBlockSourceNode isKindOf: RBBlockNode).
	self assert: inlinedBlockSourceNode equals: (RBParser parseExpression: '[ :i | ^ thisContext sourceNode ]')
]
