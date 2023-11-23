Class {
	#name : 'MCClassDefinitionTest',
	#superclass : 'MCTestCase',
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'private - accessing' }
MCClassDefinitionTest class >> classAComment [
	^ 'This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'
]

{ #category : 'private - accessing' }
MCClassDefinitionTest class >> classACommentStamp [
	^  'cwp 8/10/2003 16:43'
]

{ #category : 'utilities' }
MCClassDefinitionTest class >> restoreClassAComment [
	"This is an utility class to restore the class comment in a mock class used by the test"
	<script>
	
	self environment
		at: #MCMockClassA
		ifPresent: [ :a | a comment: self classAComment stamp: self classACommentStamp ]
]

{ #category : 'private' }
MCClassDefinitionTest >> classAComment [
	^ self class classAComment
]

{ #category : 'tests' }
MCClassDefinitionTest >> mockClass: className super: superclassName [

	^ (MCClassDefinition named: className)
		  superclassName: superclassName;
		  packageName: self packageForTestName;
		  comment: (self commentForClass: className);
		  commentStamp: (self commentStampForClass: className);
		  yourself
]

{ #category : 'private' }
MCClassDefinitionTest >> packageForTestName [
	^ 'Generated-Monticello-Tests'
]

{ #category : 'running' }
MCClassDefinitionTest >> tearDown [

	self packageOrganizer removePackage: self packageForTestName.
	super tearDown
]

{ #category : 'tests' }
MCClassDefinitionTest >> testCannotLoad [
	| d |
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
	d :=  self mockClass: 'MCMockClassC' super: 'NotAnObject'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
]

{ #category : 'tests' }
MCClassDefinitionTest >> testComparison [
	| d1 d2 d3 d4 |
	d1 := self mockClass: 'A' super: 'X'.
	d2 := self mockClass: 'A' super: 'Y'.
	d3 := self mockClass: 'B' super: 'X'.
	d4 := self mockClass: 'B' super: 'X'.
	
	self assert: (d1 isRevisionOf: d2).
	self deny: (d1 isSameRevisionAs: d2).

	self assert: (d3 isRevisionOf: d4).
	self assert: (d3 isSameRevisionAs: d4).
	
	self deny: (d1 isRevisionOf: d3).
	self deny: (d4 isRevisionOf: d2).
]

{ #category : 'tests' }
MCClassDefinitionTest >> testCreation [
	| d |
	d :=  self mockClassA asClassDefinition.
	self assert: d className equals: #MCMockClassA.
	self assert: d superclassName equals: #MCMock.
	self assert: d type equals: #normal.
	self assert: d category equals: self mockCategoryName.
	self assert: d instVarNames asArray equals: #('ivar').
	self assert: d classVarNames asArray equals: #('CVar' 'InitializationOrder').
	self assert: d classInstVarNames asArray equals: #().
	self assert: d comment isString.
	self assert: d comment equals: self classAComment.
	self assert: d commentStamp equals: self mockClassA commentStamp
]

{ #category : 'tests' }
MCClassDefinitionTest >> testDefinitionString [
	| d |
	d := self mockClassA asClassDefinition.
	self assert: d definitionString equals: self mockClassA oldDefinition.
]

{ #category : 'tests' }
MCClassDefinitionTest >> testEquals [
	| a b |
	a := self mockClass: 'ClassA' super: 'SuperA'.
	b := self mockClass: 'ClassA' super: 'SuperA'.
	self assert: a equals: b
]

{ #category : 'tests' }
MCClassDefinitionTest >> testEqualsSensitivity [

	| defA defB |
	defA := (MCClassDefinition named: 'ClassA')
		        superclassName: 'SuperA';
		        packageName: 'PackageA';
		        instVarNames: #( iVarA );
		        classVarNames: #( cVarB );
		        poolDictionaryNames: #( PoolA );
		        classInstVarNames: #( ciVarA );
		        type: #typeA;
		        comment: 'A comment';
		        yourself.

	defB := defA copy name: 'ClassB'.
	self deny: defA equals: defB.
	defB := defA copy superclassName: 'SuperB'.
	self deny: defA equals: defB.
	defB := defA copy packageName: 'PackageB'.
	self deny: defA equals: defB.
	defB := defA copy traitComposition: 'T'.
	self deny: defA equals: defB.
	defB := defA copy classTraitComposition: 'T'.
	self deny: defA equals: defB.
	defB := defA copy category: 'CategoryB'.
	self deny: defA equals: defB.
	defB := defA copy instVarNames: #( iVarB ).
	self deny: defA equals: defB.
	defB := defA copy classVarNames: #( cVarB ).
	self deny: defA equals: defB.
	defB := defA copy poolDictionaryNames: #( PoolB ).
	self deny: defA equals: defB.
	defB := defA copy classInstVarNames: #( ciVarB ).
	self deny: defA equals: defB.
	defB := defA copy type: 'typeB'.
	self deny: defA equals: defB.
	defB := defA copy comment: 'B comment'.
	self deny: defA equals: defB
]

{ #category : 'tests' }
MCClassDefinitionTest >> testKindOfSubclass [
	| classes |
	classes := {self mockClassA. String. Context. WeakArray. Float. CompiledMethod. DoubleByteArray . DoubleWordArray }.
	classes do: [:c | | d |
		d :=  c asClassDefinition.
		self assert: d kindOfSubclass equals: c kindOfSubclass.
	].
]

{ #category : 'tests' }
MCClassDefinitionTest >> testLoadAndUnload [

	| d c |
	d := self mockClass: 'MCMockClassC' super: 'Object'.
	d load.
	self assert: (testingEnvironment hasClassNamed: 'MCMockClassC').
	c := testingEnvironment classNamed: 'MCMockClassC'.
	self assert: (c isKindOf: Class).
	self assert: c superclass equals: Object.
	self assertEmpty: c instVarNames.
	self assertEmpty: c classVarNames.
	self assertEmpty: c sharedPools.
	self assert: c category equals: self packageForTestName.
	self assert: c comment equals: (self commentForClass: 'MCMockClassC').
	self assert: c commentStamp equals: (self commentStampForClass: 'MCMockClassC').
	d unload.
	self deny: (testingEnvironment hasClassNamed: 'MCMockClassC')
]

{ #category : 'tests' }
MCClassDefinitionTest >> testValidTraitComposition [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className cls |
	className := 'MCMockClassC'.
	d := self mockClass: className super: 'Object'.
	d traitComposition: '{MCTraitMock1}'.
	d load.
	self assert: (testingEnvironment hasClassNamed: 'MCMockClassC').
	cls := testingEnvironment at: #MCMockClassC.
	self assert: (cls includesSelector: #c1).
	self assert: ((testingEnvironment at: #MCMockClassC) traitComposition allTraits includes: MCTraitMock1)
]

{ #category : 'tests' }
MCClassDefinitionTest >> testValidTraitComposition2 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className class |
	className := 'MCMockClassC'.
	d := self mockClass: className super: 'Object'.
	d traitComposition: '{MCTraitMock1}'.
	d load.
	self assert: (testingEnvironment hasClassNamed: 'MCMockClassC').
	class := testingEnvironment at: #MCMockClassC.
	self assert: (class includesSelector: #c1).
	self assert: ((testingEnvironment at: #MCMockClassC) traitComposition allTraits includes: MCTraitMock1)
]

{ #category : 'tests' }
MCClassDefinitionTest >> testValidTraitComposition3 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className class |
	className := 'MCMockClassC'.
	d := self mockClass: className super: 'Object'.
	d traitComposition: 'MCTraitMock1 + MCTraitMock2'.
	d load.
	self assert: (testingEnvironment hasClassNamed: 'MCMockClassC').
	class := testingEnvironment at: #MCMockClassC.
	self assert: (class includesSelector: #c1).
	self assert: (class includesSelector: #c2).
	self assert: ((testingEnvironment at: #MCMockClassC) traitComposition allTraits includes: MCTraitMock1).
	self assert: ((testingEnvironment at: #MCMockClassC) traitComposition allTraits includes: MCTraitMock2)
]

{ #category : 'tests' }
MCClassDefinitionTest >> testValidTraitComposition4 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2598"

	| d className cls |
	className := 'MCMockClassC'.
	d := self mockClass: className super: 'Object'.
	d traitComposition: 'MCTraitMock1 - {#c1}'.

	d load.
	self assert: (testingEnvironment hasClassNamed: 'MCMockClassC').
	cls := testingEnvironment at: #MCMockClassC.
	self assert: (cls selectors includesAll: { #c }).
	self deny: (cls selectors includesAnyOf: { #c1 })
]
