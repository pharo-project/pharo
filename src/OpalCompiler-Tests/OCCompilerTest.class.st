"
CompilerTest is a holder for SUnit test of Compiler
"
Class {
	#name : #OCCompilerTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'errorMessage',
		'errorLocation',
		'errorSource',
		'interactive',
		'text'
	],
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #running }
OCCompilerTest >> compile [
	^ self compileWithFailBlock: [^ nil]
]

{ #category : #running }
OCCompilerTest >> compileWithFailBlock: aBlock [
	^ OpalCompiler new 
		source: text;
		class: MockForCompilation;
		requestor: self;
		failBlock: aBlock;
		compile.

]

{ #category : #mocking }
OCCompilerTest >> initializeErrorMessage [
	errorMessage := nil.
	errorLocation := nil.
	errorSource := nil
]

{ #category : #mocking }
OCCompilerTest >> interactive [
	^interactive
]

{ #category : #mocking }
OCCompilerTest >> notify: aString at: aSmallInteger in: aReadStream [ 
	errorMessage := aString.
	errorLocation := aSmallInteger.
	errorSource := aReadStream.
	
]

{ #category : #running }
OCCompilerTest >> runCase [
	SystemAnnouncer uniqueInstance suspendAllWhile: [ super runCase ]

]

{ #category : #running }
OCCompilerTest >> setUp [
	super setUp.
	testingEnvironment at: #OCCompilerTestTestVar put: MockForCompilation.
	
	
]

{ #category : #running }
OCCompilerTest >> tearDown [
	testingEnvironment removeKey: #OCCompilerTestTestVar.
	super tearDown
]

{ #category : #utilities }
OCCompilerTest >> tempTraitShadowingString [

	^ 'testReplaceFromToWithStartingAt
	| result  repStart collection replacementCollection firstIndex secondIndex |
		replacementCollection := 1.'
]

{ #category : #'test readonly vars' }
OCCompilerTest >> testAssignmentOfClassNameBinding [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp | | MockForCompilation := nil'.

	self
		compileWithFailBlock: [ self assert: errorMessage equals: 'Cannot store into ->'.
			self assert: errorLocation equals: 10.
			self assert: errorSource contents equals: 'temp | | MockForCompilation := nil'.
			^ nil ].
	self fail
]

{ #category : #'test readonly vars' }
OCCompilerTest >> testAssignmentOfGlobalVarBinding [
	interactive := true.
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

{ #category : #'test shadowing' }
OCCompilerTest >> testInBlockArgumentInstanceVariableShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:var1 | ]'.

	self compile.

	self flag: 'display menu instead of going into failblock'.

	self assert: errorMessage equals: 'Name already defined ->'.
	self assert: errorLocation equals: 8.
	self assert: errorSource contents equals: 'temp [:var1 | ]'
]

{ #category : #'test shadowing' }
OCCompilerTest >> testInBlockTempArgumentShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:temp | |temp|]'.

	self
		compileWithFailBlock: [ self assert: errorMessage equals: 'Name already defined ->'.
			self assert: errorLocation equals: 16.
			self assert: errorSource contents equals: 'temp [:temp | |temp|]'.
			^ nil ].
	self fail
]

{ #category : #'test shadowing' }
OCCompilerTest >> testInBlockTempInstanceVariableShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:temp | |var1|]'.

	self
		compileWithFailBlock: [ self assert: errorMessage equals: 'Name already defined ->'.
			self assert: errorLocation equals: 16.
			self assert: errorSource contents equals: 'temp [:temp | |var1|]'.
			^ nil ].
	self fail
]

{ #category : #'test shadowing' }
OCCompilerTest >> testInBlockTempShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp |var2| [:temp| |var2|]'.

	self
		compileWithFailBlock: [ self assert: errorMessage equals: 'Name already defined ->'.
			self assert: errorLocation equals: 22.
			self assert: errorSource contents equals: 'temp |var2| [:temp| |var2|]'.
			^ nil ].
	self fail
]

{ #category : #'test shadowing' }
OCCompilerTest >> testInstanceVariableShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'var1 |var1|'.

	self
		compileWithFailBlock: [ self assert: errorMessage equals: 'Name already defined ->'.
			self assert: errorLocation equals: 7.
			self assert: errorSource contents equals: 'var1 |var1|'.
			^ nil ].
	self fail
]

{ #category : #literals }
OCCompilerTest >> testNegativeZero [
	self
		assert:
			(OpalCompiler new
				source: '-0.0';
				evaluate) hex
		equals: Float negativeZero hex
]

{ #category : #'test shadowing' }
OCCompilerTest >> testNoShadowing [
	interactive := true.
	self initializeErrorMessage.
	text := 'temp |var2|  var2:=1'.

	self compileWithFailBlock: [
		self fail.
		^nil ].
	


]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveInBlockArgumentInstanceVariableShadowing [

	interactive := false.
	self initializeErrorMessage.

	self 
		should: [ 
			OpalCompiler new 
				source: 'temp [:var1 | ]';
				class: MockForCompilation;
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var1 is shadowed)'
		].

]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveInBlockTempArgumentShadowing [
	
	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp [:temp | |temp|]';
				class: MockForCompilation;
				requestor: self;
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(temp is shadowed)'
		].
]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveInBlockTempInstanceVariableShadowing [
	
	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp [:temp | |var1|]';
				class: MockForCompilation;
				requestor: self;
				failBlock: [self fail. ^ nil];
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var1 is shadowed)'
		].
	
]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveInBlockTempShadowing [
	
	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp |var2| [:temp| |var2|]';
				class: MockForCompilation;
				requestor: self;
				failBlock: [self fail];
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var2 is shadowed)'
		].


]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveNoShadowing [

	interactive := false.
	self initializeErrorMessage.

	self shouldnt: [ 
		OpalCompiler new
			source: 'temp |var2|';
			class: MockForCompilation;
			requestor: self;
			failBlock: [self fail. ^ nil ];
			compile.
	] raise: OCShadowVariableWarning.
	



]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveShadowingOfTemp [

	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp |temp1 temp1| ';
				class: MockForCompilation;
				requestor: self;
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(temp1 is shadowed)'
		].
	
]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveSiblingBlocksInstanceVariableShadowing [

	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp [:temp | ].[:temp | |var1|]';
				class:  MockForCompilation;
				requestor: self; 
				failBlock: [self fail. ^ nil ];
				compile.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var1 is shadowed)'
		].


	
]

{ #category : #'test shadowing2' }
OCCompilerTest >> testNotInteractiveSiblingBlocksTempShadowing [

	interactive := false.
	self initializeErrorMessage.

	self shouldnt: [ 
		OpalCompiler new
			source: 'temp [:temp | ]. [:temp | ]';
			class: MockForCompilation;
			requestor: self;
			failBlock: [self fail. ^nil];
			compile
	] raise: OCShadowVariableWarning.




	
					


]

{ #category : #'test shadowing' }
OCCompilerTest >> testReservedNameAsBlockArgumentShadowing [
	interactive := true.
	#('self' 'super' 'thisContext' 'true' 'false' 'nil')
		do: [ :each | 
			self initializeErrorMessage.
			[ :exit | 
			OpalCompiler new
				source: 'temp ^ [ :' , each , ' | ^ ' , each , ' ]';
				class: MockForCompilation;
				requestor: self;
				failBlock: [ exit value ];
				compile.
			self fail ] valueWithExit.
			self assert: (errorMessage = 'Variable name expected ->' or: [ errorMessage = 'Name already defined ->' ]).
			self assert: errorLocation equals: 11 ]
]

{ #category : #'test shadowing' }
OCCompilerTest >> testReservedNameAsMethodArgumentShadowing [
	interactive := true.
	#('self' 'super' 'thisContext' 'true' 'false' 'nil')
		do: [ :each | 
			self initializeErrorMessage.
			[ :exit | 
			OpalCompiler new
				source: 'temp: ' , each , ' ^ ' , each;
				class: MockForCompilation;
				requestor: self;
				failBlock: [ exit value ];
				compile.
			self fail ] valueWithExit.
			self assert: (errorMessage = 'Variable name expected ->' or: [ errorMessage = 'Name already defined ->' ]).
			self assert: errorLocation equals: 7 ]
]

{ #category : #literals }
OCCompilerTest >> testScaledDecimalLiterals [
	"Equal ScaledDecimal with different scales should use different slots
	This is related to http://bugs.squeak.org/view.php?id=6797"

	"This correctly works when evaluated separately"

	self deny: (Smalltalk compiler evaluate: '0.5s1') scale equals: (Smalltalk compiler evaluate: '0.5s2') scale.

	"But not when evaluated together if literal reduction is too agressive"
	self deny: (Smalltalk compiler evaluate: '0.5s1 scale =  0.5s2 scale')
]

{ #category : #'test shadowing' }
OCCompilerTest >> testSiblingBlocksInstanceVariableShadowing [
	interactive := true.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ].[:temp | |var1|]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [ self assert: errorMessage equals: 'Name already defined ->'.
			self assert: errorLocation equals: 27.
			^ nil ];
		compile.
	self fail
]

{ #category : #'test shadowing' }
OCCompilerTest >> testSiblingBlocksTempShadowing [

	interactive := true.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ]. [:temp | ]';
		class: MockForCompilation;
		failBlock: [self fail. ^ nil ];
		compile.
				



	
					


]

{ #category : #'test shadowing2' }
OCCompilerTest >> testTraitTempShadowing [
	
	| warningCount |
	
	warningCount := 0.
	
	self initializeErrorMessage.
	
	[ 
			OpalCompiler new 
				source: self tempTraitShadowingString;
				class: ArrayTest;
				failBlock: [self fail.];
				compile.
	] 
	on: OCShadowVariableWarning 
	do: [ :ex | 
		warningCount := warningCount + 1.
		self 
			assert: ex warningMessage 
			equals: (self traitTempShadowingWarnings at: warningCount).
		
		ex resume
	].

	self assert: warningCount equals: 3
]

{ #category : #mocking }
OCCompilerTest >> text [
	^ text
]

{ #category : #utilities }
OCCompilerTest >> traitTempShadowingWarnings [

	^ #(	
		'ArrayTest>>testReplaceFromToWithStartingAt(replacementCollection is shadowed)'
		'ArrayTest>>testReplaceFromToWithStartingAt(firstIndex is shadowed)'
		'ArrayTest>>testReplaceFromToWithStartingAt(secondIndex is shadowed)'
	)
]
