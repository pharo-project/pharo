Class {
	#name : #OCBytecodeDecompilerExamplesTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Bytecode'
}

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockArgument [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockArgument) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockArgument
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternal [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternal) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockExternal
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternal2 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternal2) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockExternal2
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternalArg [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternalArg) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockExternalArg
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternalNested [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternalNested) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockExternalNested
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockInternal [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockInternal) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockInternal
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockNested [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockNested) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleBlockNested
]

{ #category : #'tests-simple' }
OCBytecodeDecompilerExamplesTest >> testExampleEmptyMethod [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleEmptyMethod) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleEmptyMethod
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfFalse [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfFalse) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfFalse
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfFalseIfTrue [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfFalseIfTrue) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfFalseIfTrue
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfIfNotNilReturnNil [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfNotNilReturnNil) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfNotNilReturnNil
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfNotNilArg [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfNotNilArg) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfNotNilArg
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfTrue [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfTrue) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfTrue
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfTrueIfFalse [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfTrueIfFalse) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleIfTrueIfFalse
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleMethodTempInNestedBlock [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleMethodTempInNestedBlock) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleMethodTempInNestedBlock
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleMethodWithOptimizedBlocksA [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleMethodWithOptimizedBlocksA) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleMethodWithOptimizedBlocksA
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleNestedBlockScoping [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleNestedBlockScoping) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleNestedBlockScoping
]

{ #category : #'tests-simple' }
OCBytecodeDecompilerExamplesTest >> testExampleNewArray [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleNewArray) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleNewArray
]

{ #category : #'tests-misc' }
OCBytecodeDecompilerExamplesTest >> testExamplePushArray [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #examplePushArray) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance examplePushArray
]

{ #category : #'tests-simple' }
OCBytecodeDecompilerExamplesTest >> testExampleReturn1 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleReturn42) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleReturn42
]

{ #category : #'tests-simple' }
OCBytecodeDecompilerExamplesTest >> testExampleReturn1plus2 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleReturn1plus2) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleReturn1plus2
]

{ #category : #'tests-variables' }
OCBytecodeDecompilerExamplesTest >> testExampleSelf [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSelf) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSelf
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlock [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlock) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) value equals: instance exampleSimpleBlock value
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument1 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument1) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockArgument1
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument2 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument2) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockArgument2
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument3 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument3) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockArgument3
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument4 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument4) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockArgument4
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument5 [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument5) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockArgument5
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockEmpty [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockEmpty) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockEmpty
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocal [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocal) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockLocal
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalIf [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocalIf) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockLocalIf
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalNested [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockNested) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockNested
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalWhile [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocalWhile) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockLocalWhile
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockNested [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockNested) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockNested
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockReturn [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockReturn) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockReturn
]

{ #category : #'tests-blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockiVar [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockiVar) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSimpleBlockiVar
]

{ #category : #'tests-variables' }
OCBytecodeDecompilerExamplesTest >> testExampleSuper [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSuper) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleSuper
]

{ #category : #'tests-variables' }
OCBytecodeDecompilerExamplesTest >> testExampleThisContext [
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleThisContext) parseTree generate.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	instance := OCOpalExamples new.
	newMethod valueWithReceiver: instance arguments: #().
	
	self assert: instance result isContext
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoArgument [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoArgument) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoArgument
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoArgumentNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoArgumentNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoArgumentNotInlined
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoInsideTemp [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoInsideTemp) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoInsideTemp
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoInsideTempNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoInsideTempNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoInsideTempNotInlined
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoOutsideTemp [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoOutsideTemp) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoOutsideTemp
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoOutsideTempNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoOutsideTempNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoOutsideTempNotInlined
]

{ #category : #'tests-misc' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoValue [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoValue) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleToDoValue
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationAfterNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileModificationAfterNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleWhileModificationAfterNotInlined
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationBefore [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileModificationBefore) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleWhileModificationBefore
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationBeforeNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileModificationBeforeNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleWhileModificationBeforeNotInlined
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileWithTemp [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileWithTemp) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleWhileWithTemp
]

{ #category : #'tests-blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileWithTempNotInlined [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileWithTempNotInlined) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleWhileWithTempNotInlined
]

{ #category : #'tests-variables' }
OCBytecodeDecompilerExamplesTest >> testExampleiVar [
	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleiVar) parseTree generate.
	instance := OCOpalExamples new.

	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self assert: (newMethod valueWithReceiver: instance arguments: #()) equals: instance exampleiVar
]
