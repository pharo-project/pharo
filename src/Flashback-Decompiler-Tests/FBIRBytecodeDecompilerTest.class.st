Class {
	#name : 'FBIRBytecodeDecompilerTest',
	#superclass : 'TestCase',
	#instVars : [
		'currentCompiler'
	],
	#category : 'Flashback-Decompiler-Tests-Bytecode',
	#package : 'Flashback-Decompiler-Tests',
	#tag : 'Bytecode'
}

{ #category : 'running' }
FBIRBytecodeDecompilerTest >> setUp [
	super setUp.
	currentCompiler := SmalltalkImage compilerClass.
	SmalltalkImage compilerClass: OpalCompiler
]

{ #category : 'running' }
FBIRBytecodeDecompilerTest >> tearDown [
	SmalltalkImage compilerClass: currentCompiler.
	super tearDown
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testBugOffset [
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
FBIRBytecodeDecompilerTest >> testDecompileBytecodeDecompilerTestClass [
	| decompiledIR aCompiledMethod |
	self class methods
		do: [ :each |
			decompiledIR := FBIRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
FBIRBytecodeDecompilerTest >> testDecompileBytecodeGeneratorTest [
	| decompiledIR aCompiledMethod |
	OCBytecodeGeneratorTest methods
		do: [ :each |
			decompiledIR := FBIRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
FBIRBytecodeDecompilerTest >> testDecompileIRBuilderTestClass [
	| decompiledIR aCompiledMethod |
	IRBuilderTest methods
		do: [ :each |
			decompiledIR := FBIRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]
]

{ #category : 'tests - examples' }
FBIRBytecodeDecompilerTest >> testDecompilerOrderedCollectionDo [
	| cm decompiledIR |
	cm := OrderedCollection >> #do:.
	decompiledIR := FBIRBytecodeDecompiler new decompile: cm.
	decompiledIR compiledMethod
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testDup [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testDup.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testInstVar [
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
FBIRBytecodeDecompilerTest >> testJumpAheadTo [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpAheadTo.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testJumpAheadToIf [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpAheadToIf.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testJumpBackTo [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testJumpBackTo.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralArray [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralArray.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralBoolean [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralBoolean.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralCharacter [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralCharacter.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralFloat [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralFloat.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralInteger [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralInteger.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralNil [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralNil.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralString [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralString.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralSymbol [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralSymbol.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralVariableClass [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralVariableClass.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralVariableClassVariable [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralVariableClassVariable.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testLiteralVariableGlobale [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new setUp; testLiteralVariableGlobale.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPopTop [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPopTop.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushConsArray [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushConsArray.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushConsArray2 [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushConsArray2.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushSelf [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushSelf.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushTempArgument [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushTempArgument.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushTempTemp [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushTempTemp.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testPushThisContext [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testPushThisContext.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testReturnTop [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testReturnTop.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testSendSuper [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testSendSuper.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testStoreIntoVariable [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testStoreIntoVariable.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests' }
FBIRBytecodeDecompilerTest >> testStoreTemp [
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testStoreTemp.
	method := ir1 compiledMethod.
	ir2 := FBIRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod identicalTo: method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method
]

{ #category : 'tests - examples' }
FBIRBytecodeDecompilerTest >> testWhileTrue [
	| cm decompiledIR aCompiledMethod |
	cm := self class >> #testWhileTrue.
	decompiledIR := FBIRBytecodeDecompiler new decompile: cm.
	aCompiledMethod := decompiledIR compiledMethod
]
