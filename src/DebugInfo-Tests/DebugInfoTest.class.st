Class {
	#name : 'DebugInfoTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'cached'
	],
	#classVars : [
		'ClassVar'
	],
	#category : 'DebugInfo-Tests',
	#package : 'DebugInfo-Tests'
}

{ #category : 'building suites' }
DebugInfoTest class >> testParameters [

	^ ParametrizedTestMatrix new forSelector: #cached addOptions: { false. true }
]

{ #category : 'accessing' }
DebugInfoTest >> cached [

	^ cached
]

{ #category : 'accessing' }
DebugInfoTest >> cached: anObject [

	cached := anObject
]

{ #category : 'helpers' }
DebugInfoTest >> methodWithArgumentCapturingContext: anArgument [

	"anArgument is unused, but the context should capture it"
	^ thisContext copy
]

{ #category : 'helpers' }
DebugInfoTest >> methodWithArgumentCapturingFullBlockContext: anArgument [

	<compilerOptions: #(- optionCleanBlockClosure)>

	"anArgument is unused, but the context should capture it"
	^ [ self. thisContext copy ] value
]

{ #category : 'helpers' }
DebugInfoTest >> methodWithArgumentCapturingOptimizedBlockContext: anArgument [

	"anArgument is unused, but the context should capture it"
	^ true ifTrue: [ thisContext copy ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithConstantBlockReturn [

	[ 1 ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithDynamicArrayCreation [

	{ 1 + 2 . 17 }
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithFullBlockReturn [

	| stolen |
	[ stolen ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithGlobalVariableAssignment [

	ClassVar := #toto
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithImplicitReturn [

	"^ self"
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithInstanceVariableAssignment [

	"Just to test, this code is never executed"
	testSelector := #toto
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithLiterals [

	17 . #asd . 'asd' . 1.77777 . #() . nil . true . false
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithLocalVariableAssignment [

	| local |
	local := #toto
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithOptimizedSend [

	1 to: 3 do: [ :i | 17 + 42 ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithRepeat [

	[1] repeat
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithReturn [

	^ self
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithSend [

	self yourself
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithVariableReads [

	| temp |
	
	temp.
	testSelector.
	ClassVar.
	DebugInfoTest
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithWhileFalse [

	[false] whileFalse
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> methodWithWhileTrue [

	[true] whileTrue 
]

{ #category : 'helpers' }
DebugInfoTest >> newDebugInfoFor: methodToTest [

	| debugInfo |
	debugInfo := OnlineSourceDebugInfo for: methodToTest.
	cached ifTrue: [ debugInfo := debugInfo asOfflineDebugInfo ].
	^ debugInfo
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testBlockArgumentVariableIsArgument [

	| debugInfo context |
	context := [ :blockArgument | thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self assert:
		(debugInfo isArgumentNamed: #blockArgument at: context pc)
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testBlockTemporaryVariableIsNotArgument [

	| debugInfo context |
	context := [ :blockArgument |
		           | tempInBlock |
		           thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self deny: (debugInfo isArgumentNamed: #tempInBlock at: context pc)
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testDoNotListVariablesDefinedInFollowingNonSurroundingLexicalScopes [

	| debugInfo context |
	"We capture the context outside of the if statement.
	Thus, the variable defined inside the conditional block should not be visible"
	context := thisContext copy.
	true ifTrue: [
			| inOptimizedScope |
			 ].

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testDoNotListVariablesDefinedInPreviousNonSurroundingLexicalScopes [

	| debugInfo context |
	true ifTrue: [
			| inOptimizedScope |
			 ].

	"We capture the context outside of the if statement.
	Thus, the variable defined inside the conditional block should not be visible"
	context := thisContext copy.
	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListArguments [

	| debugInfo contextWithArguments |
	contextWithArguments := self methodWithArgumentCapturingContext: 17.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo variableNamesAt: contextWithArguments pc)
		equals: #( anArgument )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListBlockArguments [

	| debugInfo context |
	context := [ :blockArgument | thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context blockArgument )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListMethodTemps [

	| debugInfo context |
	context := thisContext copy.
	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListOptimizedBlockArguments [

	| debugInfo context |
	context := self ifNotNil: [ :blockArgument |
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context blockArgument inOptimizedScope )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListOuterVariables [

	| debugInfo context |
	context := [ thisContext copy ] value.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListShadowedVariablesAreNotDuplicated [

	| debugInfo context |
	context := [ "Shadowing!"
		           | debugInfo |
		           debugInfo := 17.
		           thisContext copy ] value.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( context debugInfo )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListVariablesAtFirstPCOfOptimizedContext [

	| debugInfo context |
	context := true ifTrue: [
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	"We expect the following instruction sequence:
	
	        pushThisContext (1 byte)
	        send copy (1 byte)
	  pc -> storeAndPopInto inOptimizedScope (2 bytes)
	
	Thus, we expect that the variable `inOptimizedScope` is visible in the two previous PCs too, because they are lexically in the block.
	"
	
	self
		assert: (debugInfo variableNamesAt: context pc - 1)
		equals: #( debugInfo context inOptimizedScope ).
	self
		assert: (debugInfo variableNamesAt: context pc - 2)
		equals: #( debugInfo context inOptimizedScope )
]

{ #category : 'tests - listing variables' }
DebugInfoTest >> testListVariablesAtLastPCOfOptimizedContext [

	| debugInfo context |
	context := true ifTrue: [
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self
		assert: (debugInfo variableNamesAt: context pc)
		equals: #( debugInfo context inOptimizedScope )
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testMethodArgumentVariableIsArgument [

	| debugInfo contextWithArguments |
	contextWithArguments := self methodWithArgumentCapturingContext: 17.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self assert:
		(debugInfo isArgumentNamed: #anArgument at: contextWithArguments pc)
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testMethodTemporaryVariableIsNotArgument [

	| debugInfo contextWithArguments |
	contextWithArguments := thisContext copy.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self deny: (debugInfo
			 isArgumentNamed: #contextWithArguments
			 at: contextWithArguments pc)
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testNonExistingVariableIsArgumentRaisesError [

	| debugInfo context |
	context := thisContext copy.
	debugInfo := self newDebugInfoFor: context method.

	[
		debugInfo isArgumentNamed: #notAVariable at: context pc.
		self fail ]
		on: Error
		do: [ :err |
				self
					assert: err messageText
					equals: 'Variable not found: notAVariable' ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtAllButFirstBytecodeOfOptimizedSend [

	| debugInfo methodToTest send todoPCs |
	
	"This test is only valid when shouldStopAt: does not stop at
	  - compiler only inserted stores
	  - closure creations"
	self skip.
	
	methodToTest := self class >> #methodWithOptimizedSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	"a to:do: send should stop at the to:do: point but not in the intermediate steps"
	send := methodToTest ast statements first.
	todoPCs := send methodNode pcsForNode: send.
	self assert: todoPCs size > 1.

	todoPCs allButFirst do: [ :otherPC |
		self deny: (debugInfo shouldStopAt: otherPC) ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtConstantBlockCreation [

	| debugInfo methodToTest blockNode blockCreationPC |
	
	"This test is only valid when shouldStopAt: does not stop at
	  - compiler only inserted stores
	  - closure creations"
	self skip.
	
	methodToTest := self class >> #methodWithConstantBlockReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	blockNode := methodToTest ast statements first.
	blockCreationPC := (blockNode methodNode pcsForNode: blockNode) first.

	self deny: (debugInfo shouldStopAt: blockCreationPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtDynamicArrayCreation [

	| debugInfo methodToTest dynamicArrayCreationNode arrayCreationPCs |
	methodToTest := self class >> #methodWithDynamicArrayCreation.
	debugInfo := self newDebugInfoFor: methodToTest.

	dynamicArrayCreationNode := methodToTest ast statements first.
	arrayCreationPCs := dynamicArrayCreationNode methodNode pcsForNode:
		                    dynamicArrayCreationNode.
	self assert: arrayCreationPCs notEmpty.

	self deny: (debugInfo shouldStopAt: arrayCreationPCs first)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtFullBlockCreation [

	| debugInfo methodToTest blockNode blockCreationPCs |
	
	"This test is only valid when shouldStopAt: does not stop at
	  - compiler only inserted stores
	  - closure creations"
	self skip.
	
	methodToTest := self class >> #methodWithFullBlockReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	blockNode := methodToTest ast statements first.
	"A block may have many PCs: the creation of the block but also the creation of the temp verctor and the copied vars
	None of them are stop points."
	blockCreationPCs := blockNode methodNode pcsForNode: blockNode.
	self assert: blockCreationPCs notEmpty.

	blockCreationPCs do: [ :e | self deny: (debugInfo shouldStopAt: e) ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtLiterals [

	| debugInfo methodToTest literalNodes |
	methodToTest := self class >> #methodWithLiterals.
	debugInfo := self newDebugInfoFor: methodToTest.

	literalNodes := methodToTest ast statements.
	self assert: literalNodes notEmpty.

	literalNodes do: [ :node |
			| literalPCs |
			"Some literal nodes are eliminated from the bytecode (and others not?)
		Thus, some are mapped and others not.
		
		For our purposes we don't care. If not mapped we will not stop.
		But if mapped, we should not stop on them"
			literalPCs := node methodNode pcsForNode: node.
			self assert: (literalPCs isEmpty or: [
					 (debugInfo shouldStopAt: literalPCs first) not ]) ]
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testNotStopAtVariableReads [

	| debugInfo methodToTest variableNodes |
	methodToTest := self class >> #methodWithVariableReads.
	debugInfo := self newDebugInfoFor: methodToTest.

	variableNodes := methodToTest ast statements.
	self assert: variableNodes notEmpty.

	variableNodes do: [ :node |
			| variableReadPCs |
			"Some variable nodes are eliminated from the bytecode (and others not?)
		Thus, some are mapped and others not.
		
		For our purposes we don't care. If not mapped we will not stop.
		But if mapped, we should not stop on them"
			variableReadPCs := node methodNode pcsForNode: node.
			self assert: (variableReadPCs isEmpty or: [
					 (debugInfo shouldStopAt: variableReadPCs first) not ]) ]
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testOptimizedBlockArgumentVariableIsArgument [

	| debugInfo context |
	context := self ifNotNil: [ :blockArgument |
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self assert:
		(debugInfo isArgumentNamed: #blockArgument at: context pc)
]

{ #category : 'tests - arguments' }
DebugInfoTest >> testOptimizedBlockTemporaryVariableIsNotArgument [

	| debugInfo context |
	context := self ifNotNil: [ :blockArgument |
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self deny:
		(debugInfo isArgumentNamed: #inOptimizedScope at: context pc)
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForConstantBlockReturn [

	| debugInfo methodToTest returnPC blockNode constantBlock |
	methodToTest := self class >> #methodWithConstantBlockReturn.
	constantBlock := methodToTest literalAt: 1.
	debugInfo := self newDebugInfoFor: constantBlock compiledBlock.

	"Since the return does not exist in the AST, the block return bytecode is mapped to the block's sequence node (differently than methods)"
	blockNode := methodToTest ast statements first.
	"The return bytecode is the last bytecode of the block"
	returnPC := (blockNode pcsForNode: blockNode body) last.

	self
		assert: (debugInfo rangeForPC: returnPC)
		equals: blockNode body debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForExplicitReturn [

	| debugInfo methodToTest return returnPC |
	methodToTest := self class >> #methodWithReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	return := methodToTest ast statements first.
	returnPC := (return methodNode pcsForNode: return) first.

	self
		assert: (debugInfo rangeForPC: returnPC)
		equals: return debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForFirstBytecodeOfOptimizedSend [

	| debugInfo methodToTest send sendPC |
	methodToTest := self class >> #methodWithOptimizedSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	"a to:do: send should stop at the to:do: point"
	send := methodToTest ast statements first.
	sendPC := (send methodNode pcsForNode: send) first.

	"We highlight from the beginning of #to: up to the end of #do: without selecting the receiver or the block"
	self
		assert: (debugInfo rangeForPC: sendPC)
		equals:
		(send keywordsPositions first to: send stopWithoutParentheses)
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForFullBlockReturn [

	| debugInfo methodToTest returnPC blockNode compiledBlock |
	methodToTest := self class >> #methodWithFullBlockReturn.
	compiledBlock := methodToTest literalAt: 1.
	debugInfo := self newDebugInfoFor: compiledBlock.

	"Since the return does not exist in the AST, the block return bytecode is mapped to the block's sequence node (differently than methods).
	This happens because a block node is mapped to its creation!
	However: why we don't map sequences in methods too?"
	blockNode := methodToTest ast statements first.
	"The return bytecode is the last bytecode of the block"
	returnPC := (blockNode pcsForNode: blockNode body) last.

	self
		assert: (debugInfo rangeForPC: returnPC)
		equals: blockNode body debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForGlobalVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithGlobalVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self
		assert: (debugInfo rangeForPC: returnPC)
		equals: assignmentNode debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForImplicitMethodReturn [

	| debugInfo methodToTest returnPC methodNode |
	methodToTest := self class >> #methodWithImplicitReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	"Since the return does not exist in the AST, the return bytecode is mapped to the method node"
	methodNode := methodToTest ast.
	"The return bytecode is the last bytecode of the method"
	returnPC := (methodNode pcsForNode: methodNode) last.

	"Why here the sequence node does not have a correct source mapping??"
	self
		assert: (debugInfo rangeForPC: returnPC)
		equals: methodNode debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForInstanceVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithInstanceVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode) first.
	
	self assert: (debugInfo rangeForPC: returnPC) equals: assignmentNode debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForLocalVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithLocalVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode) first.
	
	self assert: (debugInfo rangeForPC: returnPC) equals: assignmentNode debugHighlightRange
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForSend [

	| debugInfo methodToTest send sendPC |
	methodToTest := self class >> #methodWithSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	send := methodToTest ast statements first.
	sendPC := (send methodNode pcsForNode: send) first.
	
	self assert: (debugInfo rangeForPC: sendPC) equals: (send keywordsPositions first to: send stopWithoutParentheses)
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForSendInDynamicArray [

	| debugInfo methodToTest dynamicArrayNode sendInsideDynamicArrayNode sendPC |
	methodToTest := self class >> #methodWithDynamicArrayCreation.
	debugInfo := self newDebugInfoFor: methodToTest.

	dynamicArrayNode := methodToTest ast statements first.
	sendInsideDynamicArrayNode := dynamicArrayNode statements first.
	self assert: sendInsideDynamicArrayNode isMessage.

	sendPC := (sendInsideDynamicArrayNode methodNode pcsForNode: sendInsideDynamicArrayNode) first.
	
	self assert: (debugInfo rangeForPC: sendPC) equals: (sendInsideDynamicArrayNode keywordsPositions first to: sendInsideDynamicArrayNode stopWithoutParentheses)
]

{ #category : 'tests - rangeForPC' }
DebugInfoTest >> testRangeForSendInInlinedBlock [

	| debugInfo methodToTest todoSend sendInsideToDoBlock sendPC |
	methodToTest := self class >> #methodWithOptimizedSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	todoSend := methodToTest ast statements first.
	sendInsideToDoBlock := todoSend arguments second body statements first.
	self assert: sendInsideToDoBlock isMessage.
	sendPC := (sendInsideToDoBlock methodNode pcsForNode: sendInsideToDoBlock) first.
	
	self assert: (debugInfo rangeForPC: sendPC) equals: (sendInsideToDoBlock keywordsPositions first to: sendInsideToDoBlock stopWithoutParentheses)
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalBlockArgumentVariable [

	<compilerOptions: #(- optionCleanBlockClosure)>
	| debugInfo context |
	context := [ :blockArgument | thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: 7
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalBlockArgumentVariableCleanBlock [

	| debugInfo context |
	context := [ :blockArgument | thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: 7
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalBlockTempVectorTemporaryVariableFromClosure [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo |
	[ :blockArgument |
		| tempInTempVector context |
		tempInTempVector := 42.

		"Create and execute a block that steals an enclosing variable"
		"tempInTempVector is assigned in two different lexical contexts.
		It is then candidate to be put in a temp vector, which will be copied in the block activation"
		[ tempInTempVector := 3 ] value.

		context := thisContext.
		debugInfo := self newDebugInfoFor: context method.

		self
			assert:
			(debugInfo
				 readVariableNamed: #tempInTempVector
				 fromContext: context)
			equals: 3 ] value: 7.

	"Verify that the inner block got effectively executed"
	self assert: debugInfo notNil
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalBlockTempVectorTemporaryVariableFromMethod [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo tempInTempVector context |
	tempInTempVector := 42.

	"Create and execute a block that steals an enclosing variable"
	"tempInTempVector is assigned in two different lexical contexts.
		It is then candidate to be put in a temp vector, which will be copied in the block activation"
	[ tempInTempVector := 3 ] value.

	context := thisContext.
	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #tempInTempVector fromContext: context)
		equals: 3
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalBlockTemporaryVariable [

	<compilerOptions: #(- optionCleanBlockClosure)>
	| debugInfo context |
	context := [ :blockArgument |
		           | tempInBlock |
		           tempInBlock := 42.
		           thisContext copy ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #tempInBlock fromContext: context)
		equals: 42
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalMethodArgumentVariable [

	| debugInfo contextWithArguments |
	contextWithArguments := self methodWithArgumentCapturingContext: 17.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo
				 readVariableNamed: #anArgument
				 fromContext: contextWithArguments)
		equals: 17
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalMethodTemporaryVariable [

	| debugInfo contextWithArguments |
	contextWithArguments := thisContext copy.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		deny: (debugInfo
				 readVariableNamed: #contextWithArguments
				 fromContext: contextWithArguments)
		equals: contextWithArguments
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalMethodTemporaryVariableFromDeadContext [

	| debugInfo contextWithArguments |
	contextWithArguments := thisContext copy
		pc: nil;
		yourself.
	self assert: contextWithArguments isDead.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		shouldnt: [
			debugInfo
				 readVariableNamed: #contextWithArguments
				 fromContext: contextWithArguments ]
		raise: MessageNotUnderstood
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalOptimizedBlockArgumentVariable [

	| debugInfo context |
	context := self ifNotNil: [ :blockArgument |
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: self
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadLocalOptimizedBlockTemporaryVariable [

	| debugInfo context |
	context := self ifNotNil: [ :blockArgument |
			           | inOptimizedScope |
			           "Put an assignment just after the context capture.
		Like this, the captured context PC will be inside the block (at the assignment point).
		Thus, lexically we are inside the block and the variables of the block are visible"
			           inOptimizedScope := thisContext copy ].

	debugInfo := self newDebugInfoFor: context method.

	self
		deny:
		(debugInfo readVariableNamed: #inOptimizedScope fromContext: context)
		equals: context
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadNonExistingVariableRaisesError [

	| debugInfo context |
	context := thisContext copy.
	debugInfo := self newDebugInfoFor: context method.

	[
		debugInfo readVariableNamed: #notAVariable fromContext: context.
		self fail ]
		on: Error
		do: [ :err |
				self
					assert: err messageText
					equals: 'Variable not found: notAVariable' ]
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockArgumentVariableFromClosure [

	<compilerOptions: #(- optionCleanBlockClosure)>
	| debugInfo context |
	context := [ :blockArgument | [ thisContext copy ] value ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: 7
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockArgumentVariableFromOptimizedBlock [

	<compilerOptions: #(- optionCleanBlockClosure)>
	| debugInfo context |
	context := [ :blockArgument | true ifTrue: [ thisContext copy ] ]
		           value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: 7
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockCopiedArgumentVariableFromClosure [

	| debugInfo context |
	context := [ :blockArgument |
		           [
			           blockArgument.
			           thisContext copy ] value ] value: 7.

	debugInfo := self newDebugInfoFor: context method.

	self
		assert:
		(debugInfo readVariableNamed: #blockArgument fromContext: context)
		equals: 7
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockCopiedTemporaryVariableFromClosure [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo |
	[ :blockArgument |
		| tempInBlock |
		tempInBlock := 42.
		[
			| context |
			"tempInBlock is candidate to be copied in the block activation"
			tempInBlock.

			context := thisContext.
			debugInfo := self newDebugInfoFor: context method.

			self
				assert:
				(debugInfo readVariableNamed: #tempInBlock fromContext: context)
				equals: 42 ] value ] value: 7.

	"Verify that the inner block got effectively executed"
	self assert: debugInfo notNil
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockCopiedTemporaryVariableFromClosureWithDeadOuter [
	"This test is tricky.
	
	This is a variation of testReadOuterBlockCopiedTemporaryVariableFromClosure where the outer context is dead.
	The value was copied into the current context so, we should be able to read it!
	"

	| debugInfo context |
	context := [ :blockArgument |
		           | tempInBlock |
		           tempInBlock := 42.
		           [ "tempInBlock is candidate to be copied in the block activation"
			           tempInBlock.

			           thisContext copy ] value ] value: 7.

	debugInfo := self newDebugInfoFor: context method.
	self assert: (debugInfo readVariableNamed: #tempInBlock fromContext: context) equals: 42
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockTempVectorTemporaryVariableFromClosure [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo |
	[ :blockArgument |
		| tempInTempVector |
		tempInTempVector := 42.
		[
			| context |
			"tempInTempVector is assigned in two different lexical contexts.
			It is then candidate to be put in a temp vector, which will be copied in the block activation"
			tempInTempVector := 3.

			context := thisContext.
			debugInfo := self newDebugInfoFor: context method.

			self
				assert:
				(debugInfo
					 readVariableNamed: #tempInTempVector
					 fromContext: context)
				equals: 3 ] value ] value: 7.

	"Verify that the inner block got effectively executed"
	self assert: debugInfo notNil
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockTemporaryVariableFromClosure [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo |
	[ :blockArgument |
		| tempInBlock |
		tempInBlock := 42.
		[
			| context |
			context := thisContext.
			debugInfo := self newDebugInfoFor: context method.

			self
				assert:
				(debugInfo readVariableNamed: #tempInBlock fromContext: context)
				equals: 42 ] value ] value: 7.

	"Verify that the inner block got effectively executed"
	self assert: debugInfo notNil
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterBlockTemporaryVariableFromOptimizedBlock [
	"This test is tricky.
	
	If done like the other tests (copying and returning the captured context) this breaks the closures.
	The block contexts refer to closures, and each closure refer to its (creator) outer context.
	However, when copying the stack, they point to the original context and not the copy.
	
	Instead of making a transitive closure of the graph copy, let's just put the assertion in the block
	"

	| debugInfo |
	[ :blockArgument |
		| tempInBlock |
		tempInBlock := 42.
		true ifTrue: [
				| context |
				context := thisContext.
				debugInfo := self newDebugInfoFor: context method.

				self
					assert:
					(debugInfo readVariableNamed: #tempInBlock fromContext: context)
					equals: 42 ] ] value: 7.

	"Verify that the inner block got effectively executed"
	self assert: debugInfo notNil
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterMethodArgumentVariableFromClosure [

	| debugInfo contextWithArguments |
	contextWithArguments := self
		                        methodWithArgumentCapturingFullBlockContext:
		                        17.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo
				 readVariableNamed: #anArgument
				 fromContext: contextWithArguments)
		equals: 17
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterMethodArgumentVariableFromOptimizedBlock [

	| debugInfo contextWithArguments |
	contextWithArguments := self
		                        methodWithArgumentCapturingOptimizedBlockContext:
		                        17.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo
				 readVariableNamed: #anArgument
				 fromContext: contextWithArguments)
		equals: 17
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterMethodTemporaryVariableFromClosure [

	<compilerOptions: #(- optionCleanBlockClosure)>
	
	| debugInfo contextWithArguments |
	contextWithArguments := [ thisContext copy ] value.
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo
				 readVariableNamed: #contextWithArguments
				 fromContext: contextWithArguments)
		equals: contextWithArguments
]

{ #category : 'tests - read variables' }
DebugInfoTest >> testReadOuterMethodTemporaryVariableFromOptimizedBlock [

	| debugInfo contextWithArguments outerVariable |
	outerVariable := 17.

	contextWithArguments := true ifTrue: [ thisContext copy ].
	debugInfo := self newDebugInfoFor: contextWithArguments method.

	self
		assert: (debugInfo
				 readVariableNamed: #outerVariable
				 fromContext: contextWithArguments)
		equals: outerVariable
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtConstantBlockReturn [

	| debugInfo methodToTest returnPC blockNode constantBlock |
	methodToTest := self class >> #methodWithConstantBlockReturn.
	constantBlock := methodToTest literalAt: 1.
	debugInfo := self newDebugInfoFor: constantBlock compiledBlock.

	"Since the return does not exist in the AST, the block return bytecode is mapped to the block's sequence node (differently than methods)"
	blockNode := methodToTest ast statements first.
	"The return bytecode is the last bytecode of the block"
	returnPC := (blockNode pcsForNode: blockNode body) last.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtExplicitReturn [

	| debugInfo methodToTest return returnPC |
	methodToTest := self class >> #methodWithReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	return := methodToTest ast statements first.
	returnPC := (return methodNode pcsForNode: return) first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtFirstBytecodeOfOptimizedSend [

	| debugInfo methodToTest send sendPC |
	methodToTest := self class >> #methodWithOptimizedSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	"a to:do: send should stop at the to:do: point"
	send := methodToTest ast statements first.
	sendPC := (send methodNode pcsForNode: send) first.

	self assert: (debugInfo shouldStopAt: sendPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtFullBlockReturn [

	| debugInfo methodToTest returnPC blockNode compiledBlock |
	methodToTest := self class >> #methodWithFullBlockReturn.
	compiledBlock := methodToTest literalAt: 1.
	debugInfo := self newDebugInfoFor: compiledBlock.

	"Since the return does not exist in the AST, the block return bytecode is mapped to the block's sequence node (differently than methods).
	This happens because a block node is mapped to its creation!
	However: why we don't map sequences in methods too?"
	blockNode := methodToTest ast statements first.
	"The return bytecode is the last bytecode of the block"
	returnPC := (blockNode pcsForNode: blockNode body) last.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtGlobalVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithGlobalVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtImplicitMethodReturn [

	| debugInfo methodToTest returnPC methodNode |
	methodToTest := self class >> #methodWithImplicitReturn.
	debugInfo := self newDebugInfoFor: methodToTest.

	"Since the return does not exist in the AST, the return bytecode is mapped to the method node"
	methodNode := methodToTest ast.
	"The return bytecode is the last bytecode of the method"
	returnPC := (methodNode pcsForNode: methodNode) last.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtInstanceVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithInstanceVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtLocalVariableAssignment [

	| debugInfo methodToTest returnPC assignmentNode |
	methodToTest := self class >> #methodWithLocalVariableAssignment.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtRepeat [
	
	| debugInfo methodToTest assignmentNode returnPC |
	methodToTest := self class >> #methodWithRepeat.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtSend [

	| debugInfo methodToTest send sendPC |
	methodToTest := self class >> #methodWithSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	send := methodToTest ast statements first.
	sendPC := (send methodNode pcsForNode: send) first.

	self assert: (debugInfo shouldStopAt: sendPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtSendInDynamicArray [

	| debugInfo methodToTest dynamicArrayNode sendInsideDynamicArrayNode sendPC |
	methodToTest := self class >> #methodWithDynamicArrayCreation.
	debugInfo := self newDebugInfoFor: methodToTest.

	dynamicArrayNode := methodToTest ast statements first.
	sendInsideDynamicArrayNode := dynamicArrayNode statements first.
	self assert: sendInsideDynamicArrayNode isMessage.

	sendPC := (sendInsideDynamicArrayNode methodNode pcsForNode:
		           sendInsideDynamicArrayNode) first.

	self assert: (debugInfo shouldStopAt: sendPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtSendInInlinedBlock [

	| debugInfo methodToTest todoSend sendInsideToDoBlock sendPC |
	methodToTest := self class >> #methodWithOptimizedSend.
	debugInfo := self newDebugInfoFor: methodToTest.

	todoSend := methodToTest ast statements first.
	sendInsideToDoBlock := todoSend arguments second body statements
		                       first.
	self assert: sendInsideToDoBlock isMessage.
	sendPC := (sendInsideToDoBlock methodNode pcsForNode:
		           sendInsideToDoBlock) first.

	self assert: (debugInfo shouldStopAt: sendPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtWhileFalse [

	| debugInfo methodToTest assignmentNode returnPC |
	methodToTest := self class >> #methodWithWhileFalse.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]

{ #category : 'tests - shouldStopAt' }
DebugInfoTest >> testStopAtWhileTrue [

	| debugInfo methodToTest assignmentNode returnPC |
	methodToTest := self class >> #methodWithWhileTrue.
	debugInfo := self newDebugInfoFor: methodToTest.

	assignmentNode := methodToTest ast statements first.
	returnPC := (assignmentNode methodNode pcsForNode: assignmentNode)
		            first.

	self assert: (debugInfo shouldStopAt: returnPC)
]
