Class {
	#name : 'OCBytecodeDecompilerTest',
	#superclass : 'TestCase',
	#instVars : [
		'currentCompiler'
	],
	#category : 'OpalCompiler-Tests-Bytecode',
	#package : 'OpalCompiler-Tests',
	#tag : 'Bytecode'
}

{ #category : 'running' }
OCBytecodeDecompilerTest >> setUp [
	super setUp.
	currentCompiler := SmalltalkImage compilerClass.
	SmalltalkImage compilerClass: OpalCompiler
]

{ #category : 'running' }
OCBytecodeDecompilerTest >> tearDown [
	SmalltalkImage compilerClass: currentCompiler.
	super tearDown
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testBugOffset [
	| iRMethod aCompiledMethod ir method |
	iRMethod := IRBuilder new
		addTemps: #(#a #b);
		pushTemp: #b;
		pushTemp: #a;
		returnTop;
		ir.
	aCompiledMethod := iRMethod compiledMethod.
	ir := aCompiledMethod decompileIR.
	method := ir compiledMethod.
	self assert: aCompiledMethod symbolic equals: method symbolic
]

{ #category : 'tests - examples' }
OCBytecodeDecompilerTest >> testDecompileBytecodeDecompilerTestClass [
	| decompiledIR aCompiledMethod |
	self class methods
		do: [ :each |
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
OCBytecodeDecompilerTest >> testDecompileBytecodeGeneratorTest [
	| decompiledIR aCompiledMethod |
	OCBytecodeGeneratorTest methods
		do: [ :each |
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
OCBytecodeDecompilerTest >> testDecompileIRBuilderTestClass [
	| decompiledIR aCompiledMethod |
	IRBuilderTest methods
		do: [ :each |
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
OCBytecodeDecompilerTest >> testDecompilerOrderedCollectionDo [
	| cm decompiledIR |
	cm := OrderedCollection >> #do:.
	decompiledIR := IRBytecodeDecompiler new decompile: cm.
	decompiledIR compiledMethod
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testDup [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testDup.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testInstVar [
	| ir1 ir2 method method2 |
	ir1 := IRBuilderTest new testInstVar.
	method := ir1 compiledMethod.
	ir2 := method decompileIR.
	method2 := ir2 compiledMethod.
	self deny: method2 identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: method2 equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testJumpAheadTo [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpAheadTo.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testJumpAheadToIf [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpAheadToIf.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testJumpBackTo [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpBackTo.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralArray [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralArray.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralBoolean [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralBoolean.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralCharacter [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralCharacter.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralFloat [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralFloat.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralInteger [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralInteger.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralNil [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralNil.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralString [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralString.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralSymbol [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralSymbol.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralVariableClass [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralVariableClass.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralVariableClassVariable [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralVariableClassVariable.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testLiteralVariableGlobale [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new setUp; testLiteralVariableGlobale.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPopTop [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPopTop.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushConsArray [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushConsArray.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushConsArray2 [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushConsArray2.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushSelf [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushSelf.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushTempArgument [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushTempArgument.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushTempTemp [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushTempTemp.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testPushThisContext [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushThisContext.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testReturnTop [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testReturnTop.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testSendSuper [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testSendSuper.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testStoreIntoVariable [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testStoreIntoVariable.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
OCBytecodeDecompilerTest >> testStoreTemp [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testStoreTemp.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests - examples' }
OCBytecodeDecompilerTest >> testWhileTrue [
	| cm decompiledIR aCompiledMethod |
	cm := self class >> #testWhileTrue.
	decompiledIR := IRBytecodeDecompiler new decompile: cm.
	aCompiledMethod := decompiledIR compiledMethod
]
