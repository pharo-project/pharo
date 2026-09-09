"
CompilerTest is a holder for SUnit test of Compiler
"
Class {
	#name : 'OCCompilerTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'errorMessage',
		'errorLocation',
		'errorSource',
		'text'
	],
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'running' }
OCCompilerTest >> compile [
	^ self compileWithFailBlock: [^ nil]
]

{ #category : 'running' }
OCCompilerTest >> compileWithFailBlock: aBlock [

	| result compiler |
	compiler := MockForCompilation compiler
		            source: text;
		            "compatibility: permit undeclared if no requestor"
		            permitUndeclared: false;
		            permitFaulty: false.
	result := compiler doCompile.
	result isError ifTrue: [ aBlock cull: result ]
]

{ #category : 'mocking' }
OCCompilerTest >> initializeErrorMessage [
	errorMessage := nil.
	errorLocation := nil.
	errorSource := nil
]

{ #category : 'mocking' }
OCCompilerTest >> notify: aString at: aSmallInteger in: aReadStream [
	errorMessage := aString.
	errorLocation := aSmallInteger.
	errorSource := aReadStream
]

{ #category : 'running' }
OCCompilerTest >> runCase [

	self class codeChangeAnnouncer suspendAllWhile: [ super runCase ]
]

{ #category : 'running' }
OCCompilerTest >> setUp [
	super setUp.
	testingEnvironment at: #OCCompilerTestTestVar put: MockForCompilation
]

{ #category : 'running' }
OCCompilerTest >> tearDown [
	testingEnvironment removeKey: #OCCompilerTestTestVar.
	super tearDown
]

{ #category : 'utilities' }
OCCompilerTest >> tempTraitShadowingString [

	^ 'testReplaceFromToWithStartingAt
	| result  repStart collection replacementCollection firstIndex secondIndex |
		replacementCollection := 1.'
]

{ #category : 'tests - readonly vars' }
OCCompilerTest >> testAssignmentOfClassNameBinding [
	self initializeErrorMessage.
	text := 'temp | | MockForCompilation := nil'.

	self
		compileWithFailBlock: [ :result | | error |
			self assert: result ast allNotices size equals: 1.
			error := result ast allNotices first.
			self assert: error messageText equals: 'Assignment to read-only variable'.
			self assert: error position equals: 10.
			self assert: error node parent sourceCode equals: 'MockForCompilation := nil'.
			^ nil ].
	self fail
]

{ #category : 'tests - readonly vars' }
OCCompilerTest >> testAssignmentOfGlobalVarBinding [

	text := 'temp | | OCCompilerTestTestVar := 2'.

	self
		should: [
				OpalCompiler new
					source: text;
					class: MockForCompilation;
					compileOrFail ]
		raise: OCCompilationError
		with: [ :err | self assert: (err messageText beginsWith: 'Compilation failed with following errors:') ]
]

{ #category : 'tests' }
OCCompilerTest >> testErrorOrders [

	| ast notices |
	ast := OpalCompiler new parse: 'foo | b | ^ a + ¿. 5 "what'.
	notices := ast allNotices sorted collect: [ :e | e position asString , ':' , e messageText ] as: Array.
	self
		assertCollection: notices
		equals: #( '17:Unknown character' '27:Unmatched " in comment.' '13:Undeclared variable named a in foo' '20:Unreachable statement' '7:Unused variable' )
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInBlockArgumentInstanceVariableShadowing [
	self initializeErrorMessage.
	text := 'temp [:var1 | ]'.

	self compile.

	self assert: errorMessage equals: nil.
	self assert: errorLocation equals: nil.
	self assert: errorSource equals: nil
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInBlockArgumentPseudoVariableShadowing [
	self initializeErrorMessage.
	text := 'temp [:thisProcess | ]'.

	self compile.

	self assert: errorMessage equals: nil.
	self assert: errorLocation equals: nil.
	self assert: errorSource equals: nil
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInBlockTempArgumentShadowing [

	self initializeErrorMessage.
	text := 'temp [:temp | |temp|]'.

	self compileWithFailBlock: [ ^ self fail ]
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInBlockTempInstanceVariableShadowing [

	self initializeErrorMessage.
	text := 'temp [:temp | |var1|]'.

	self compileWithFailBlock: [ ^ self fail ]
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInBlockTempShadowing [

	self initializeErrorMessage.
	text := 'temp |var2| [:temp| |var2|]'.

	self compileWithFailBlock: [ ^ self fail ]
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testInstanceVariableShadowing [

	self initializeErrorMessage.
	text := 'var1 |var1|'.

	self compileWithFailBlock: [ ^ self fail ]
]

{ #category : 'literals' }
OCCompilerTest >> testNegativeZero [
	self
		assert:
			(OpalEvaluator new
				evaluate: '-0.0') hex
		equals: Float negativeZero hex
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testNoShadowing [
	self initializeErrorMessage.
	text := 'temp |var2|  var2:=1'.

	self compileWithFailBlock: [
		self fail.
		^nil ]
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testPseudoVariableShadowing [

	self initializeErrorMessage.
	text := 'var1 |thisContext| ^thisContext'.

	self compile.

	self assert: errorMessage equals: nil.
	self assert: errorLocation equals: nil.
	self assert: errorSource equals: nil
]

{ #category : 'literals' }
OCCompilerTest >> testScaledDecimalLiterals [
	"Equal ScaledDecimal with different scales should use different slots"

	"This correctly works when evaluated separately"

	self 
		deny: (OpalEvaluator new evaluate: '0.5s1') scale 
		equals: (OpalEvaluator new evaluate: '0.5s2') scale.

	"But not when evaluated together if literal reduction is too agressive"
	self deny: (OpalEvaluator new evaluate: '0.5s1 scale =  0.5s2 scale')
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testSiblingBlocksInstanceVariableShadowing [
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ].[:temp | |var1|]';
		class: MockForCompilation;
		compileOrFail
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testSiblingBlocksTempShadowing [

	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ]. [:temp | ]';
		class: MockForCompilation;
		compileOrFail
]

{ #category : 'tests' }
OCCompilerTest >> testUndefinedVariableFrontend [
	"This test shows that undefined variables are registered only on backend"

	Undeclared removeKey: #undefinedName123 ifAbsent: [ ].
	OpalCompiler new parse: 'foo ^undefinedName123'.
	self deny: (Undeclared includesKey: #undefinedName123).
	self should: [ OpalCompiler new source: 'foo ^undefinedName123 ¿ 2'; compileOrFail ] raise: OCCompilationError.
	self deny: (Undeclared includesKey: #undefinedName123).
	OpalCompiler new permitFaulty: true; source: 'foo ^undefinedName123'; compileOrFail.
	self assert: (Undeclared includesKey: #undefinedName123).

	"Cleanup"
	Undeclared removeKey: #undefinedName123 ifAbsent: [ ]
]

{ #category : 'mocking' }
OCCompilerTest >> text [
	^ text
]

{ #category : 'utilities' }
OCCompilerTest >> traitTempShadowingWarnings [

	^ #(
		'ArrayTest>>testReplaceFromToWithStartingAt(replacementCollection is shadowed)'
		'ArrayTest>>testReplaceFromToWithStartingAt(firstIndex is shadowed)'
		'ArrayTest>>testReplaceFromToWithStartingAt(secondIndex is shadowed)'
	)
]
