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
		'text',
		'interactive'
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
	^ OpalCompiler new
		source: text;
		class: MockForCompilation;
		requestor: self;
		failBlock: aBlock;
		compile
]

{ #category : 'mocking' }
OCCompilerTest >> initializeErrorMessage [
	errorMessage := nil.
	errorLocation := nil.
	errorSource := nil
]

{ #category : 'accessing' }
OCCompilerTest >> interactive [

	^ interactive ifNil: [ true ]
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
		compileWithFailBlock: [ self assert: errorMessage equals: 'Assignment to read-only variable ->'.
			self assert: errorLocation equals: 10.
			self assert: errorSource contents equals: 'temp | | MockForCompilation := nil'.
			^ nil ].
	self fail
]

{ #category : 'tests - readonly vars' }
OCCompilerTest >> testAssignmentOfGlobalVarBinding [
	self initializeErrorMessage.
	text := 'temp | | OCCompilerTestTestVar := 2'.
	self initializeErrorMessage.
	OpalCompiler new
		source: text;
		class: MockForCompilation;
		requestor: self;
		failBlock: [ self fail.
			^ nil ];
		compile
]

{ #category : 'tests' }
OCCompilerTest >> testErrorOrders [

	| ast notices |
	ast := OpalCompiler new parse: 'foo | b | ^ a + ¿. 5 "what'.
	notices := ast allNotices sorted
		           collect: [ :e |
		           e position asString , ':' , e messageText ]
		           as: Array.
	self
		assertCollection: notices
		equals: #( '17:Unknown character' '27:Unmatched " in comment.'
			   '13:Undeclared variable' '7:Unused variable' '20:Unreachable statement' )
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
			(OpalCompiler new
				source: '-0.0';
				evaluate) hex
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
	"Equal ScaledDecimal with different scales should use different slots
	This is related to http://bugs.squeak.org/view.php?id=6797"

	"This correctly works when evaluated separately"

	self deny: (Smalltalk compiler evaluate: '0.5s1') scale equals: (Smalltalk compiler evaluate: '0.5s2') scale.

	"But not when evaluated together if literal reduction is too agressive"
	self deny: (Smalltalk compiler evaluate: '0.5s1 scale =  0.5s2 scale')
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testSiblingBlocksInstanceVariableShadowing [
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ].[:temp | |var1|]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [ ^ self fail ];
		compile
]

{ #category : 'tests - shadowing' }
OCCompilerTest >> testSiblingBlocksTempShadowing [

	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ]. [:temp | ]';
		class: MockForCompilation;
		compile
]

{ #category : 'tests' }
OCCompilerTest >> testUndefinedVariable [
	"This test shows that undefined variables behave as globals (for now) and is independent of the compilation mode"

	Undeclared removeKey: #undefinedName123 ifAbsent: [ ].
	self
		assert: ( [ {
			OpalCompiler new
				permitFaulty: true;
				evaluate: 'undefinedName123'.
			OpalCompiler new
				permitFaulty: true;
				evaluate: 'undefinedName123 := 1'.
			OpalCompiler new
				permitFaulty: true;
				evaluate: 'undefinedName123 := 2. undefinedName123'.
			OpalCompiler new
				permitFaulty: true;
				evaluate: 'undefinedName123' }  ] on: UndeclaredVariableRead , UndeclaredVariableWrite do: [ :e | e resume: e class ] )
		equals: { UndeclaredVariableRead. UndeclaredVariableWrite. UndeclaredVariableRead. UndeclaredVariableRead }.

	"Cleanup"
	Undeclared removeKey: #undefinedName123 ifAbsent: [ ]
]

{ #category : 'tests' }
OCCompilerTest >> testUndefinedVariableFrontend [
	"This test shows that undefined variables are registered only on backend"

	Undeclared removeKey: #undefinedName123 ifAbsent: [ ].
	OpalCompiler new parse: 'foo ^undefinedName123'.
	self deny: (Undeclared includesKey: #undefinedName123).
	self should: [ OpalCompiler new compile: 'foo ^undefinedName123 ¿ 2' ] raise: CodeError.
	self deny: (Undeclared includesKey: #undefinedName123).
	OpalCompiler new permitFaulty: true; compile: 'foo ^undefinedName123'.
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
