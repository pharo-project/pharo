Class {
	#name : 'OCBytecodeDecompilerExamplesTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Bytecode',
	#package : 'OpalCompiler-Tests',
	#tag : 'Bytecode'
}

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockArgument [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockArgument) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockArgument
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternal [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternal) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockExternal
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternal2 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternal2) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockExternal2
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternalArg [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternalArg) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockExternalArg
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockExternalNested [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockExternalNested) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockExternalNested
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockInternal [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockInternal) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockInternal
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleBlockNested [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleBlockNested) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleBlockNested
]

{ #category : 'tests - simple' }
OCBytecodeDecompilerExamplesTest >> testExampleEmptyMethod [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleEmptyMethod) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleEmptyMethod
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfFalse [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfFalse) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfFalse
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfFalseIfTrue [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfFalseIfTrue) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfFalseIfTrue
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfIfNotNilReturnNil [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfNotNilReturnNil) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfNotNilReturnNil
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfNotNilArg [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfNotNilArg) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfNotNilArg
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfTrue [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfTrue) parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfTrue
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleIfTrueIfFalse [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleIfTrueIfFalse) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleIfTrueIfFalse
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleMethodTempInNestedBlock [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleMethodTempInNestedBlock)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleMethodTempInNestedBlock
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleMethodWithOptimizedBlocksA [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleMethodWithOptimizedBlocksA)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleMethodWithOptimizedBlocksA
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleNestedBlockScoping [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleNestedBlockScoping) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleNestedBlockScoping
]

{ #category : 'tests - simple' }
OCBytecodeDecompilerExamplesTest >> testExampleNewArray [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleNewArray) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleNewArray
]

{ #category : 'tests - misc' }
OCBytecodeDecompilerExamplesTest >> testExamplePushArray [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #examplePushArray) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance examplePushArray
]

{ #category : 'tests - simple' }
OCBytecodeDecompilerExamplesTest >> testExampleReturn1 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleReturn42) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleReturn42
]

{ #category : 'tests - simple' }
OCBytecodeDecompilerExamplesTest >> testExampleReturn1plus2 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleReturn1plus2) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleReturn1plus2
]

{ #category : 'tests - variables' }
OCBytecodeDecompilerExamplesTest >> testExampleSelf [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSelf) parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSelf
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlock [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlock) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert:
		(newMethod valueWithReceiver: instance) value
		equals: instance exampleSimpleBlock value
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument1 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument1) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockArgument1
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument2 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument2) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockArgument2
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument3 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument3) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockArgument3
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument4 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument4) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockArgument4
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockArgument5 [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockArgument5) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockArgument5
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockEmpty [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockEmpty) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockEmpty
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocal [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocal) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockLocal
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalIf [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocalIf) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockLocalIf
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalNested [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockNested) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockNested
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockLocalWhile [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockLocalWhile) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockLocalWhile
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockNested [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockNested) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockNested
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockReturn [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockReturn) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockReturn
]

{ #category : 'tests - blocks' }
OCBytecodeDecompilerExamplesTest >> testExampleSimpleBlockiVar [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSimpleBlockiVar) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSimpleBlockiVar
]

{ #category : 'tests - variables' }
OCBytecodeDecompilerExamplesTest >> testExampleSuper [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleSuper) parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleSuper
]

{ #category : 'tests - variables' }
OCBytecodeDecompilerExamplesTest >> testExampleThisContext [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleThisContext) parseTree
		          generateMethod.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	instance := OCOpalExamples new.
	newMethod valueWithReceiver: instance.

	self assert: instance result isContext
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoArgument [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoArgument) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoArgument
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoArgumentNotInlined [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoArgumentNotInlined)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoArgumentNotInlined
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoInsideTemp [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoInsideTemp) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoInsideTemp
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoInsideTempNotInlined [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoInsideTempNotInlined)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoInsideTempNotInlined
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoOutsideTemp [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoOutsideTemp) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoOutsideTemp
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoOutsideTempNotInlined [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoOutsideTempNotInlined)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoOutsideTempNotInlined
]

{ #category : 'tests - misc' }
OCBytecodeDecompilerExamplesTest >> testExampleToDoValue [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleToDoValue) parseTree
		          generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleToDoValue
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationAfterNotInlined [

	| ir method newMethod |
	method := (OCOpalExamples >> #exampleWhileModificationAfterNotInlined)
		          parseTree generateMethod.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert:
		(newMethod valueWithReceiver: OCOpalExamples new)
		equals: OCOpalExamples new exampleWhileModificationAfterNotInlined
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationBefore [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleWhileModificationBefore)
		          parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleWhileModificationBefore
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileModificationBeforeNotInlined [

	| ir method newMethod |
	method := (OCOpalExamples
	           >> #exampleWhileModificationBeforeNotInlined) parseTree
		          generateMethod.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert:
		(newMethod valueWithReceiver: OCOpalExamples new)
		equals: OCOpalExamples new exampleWhileModificationBeforeNotInlined
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileWithTemp [

	| ir method newMethod |
	method := (OCOpalExamples >> #exampleWhileWithTemp) parseTree
		          generateMethod.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert:
		(newMethod valueWithReceiver: OCOpalExamples new)
		equals: OCOpalExamples new exampleWhileWithTemp
]

{ #category : 'tests - blocks-optimized' }
OCBytecodeDecompilerExamplesTest >> testExampleWhileWithTempNotInlined [

	| ir method newMethod |
	method := (OCOpalExamples >> #exampleWhileWithTempNotInlined)
		          parseTree generateMethod.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert:
		(newMethod valueWithReceiver: OCOpalExamples new)
		equals: OCOpalExamples new exampleWhileWithTempNotInlined
]

{ #category : 'tests - variables' }
OCBytecodeDecompilerExamplesTest >> testExampleiVar [

	| ir method newMethod instance |
	method := (OCOpalExamples >> #exampleiVar) parseTree generateMethod.
	instance := OCOpalExamples new.

	ir := FBIRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.

	self
		assert: (newMethod valueWithReceiver: instance)
		equals: instance exampleiVar
]
