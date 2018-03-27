Class {
	#name : #MCClassDefinitionTest,
	#superclass : #MCTestCase,
	#instVars : [
		'previousChangeSet'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #'private - accessing' }
MCClassDefinitionTest class >> classAComment [
	^ 'This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'
]

{ #category : #'private - accessing' }
MCClassDefinitionTest class >> classACommentStamp [
	^  'cwp 8/10/2003 16:43'
]

{ #category : #utilities }
MCClassDefinitionTest class >> restoreClassAComment [
	"This is an utility class to restore the class comment in a mock class used by the test"
	<script>
	
	Smalltalk globals
		at: #MCMockClassA
		ifPresent: [ :a | a classComment: self classAComment stamp: self classACommentStamp ]
]

{ #category : #private }
MCClassDefinitionTest >> classAComment [
	^ self class classAComment
]

{ #category : #private }
MCClassDefinitionTest >> creationMessage [
	^ MessageSend
		receiver: MCClassDefinition
		selector: #name:superclassName:category:instVarNames:classVarNames:poolDictionaryNames:classInstVarNames:type:comment:commentStamp:
]

{ #category : #running }
MCClassDefinitionTest >> setUp [
	super setUp.
	Smalltalk globals at: #MCMockClassC ifPresent: [ :c | c removeFromSystem ]
]

{ #category : #running }
MCClassDefinitionTest >> tearDown [
	Smalltalk globals at: #MCMockClassC ifPresent: [ :c | c removeFromSystem ].
	super tearDown
]

{ #category : #tests }
MCClassDefinitionTest >> testCannotLoad [
	| d |
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
	d :=  self mockClass: 'MCMockClassC' super: 'NotAnObject'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
]

{ #category : #tests }
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

{ #category : #tests }
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
	self assert: d commentStamp equals: self mockClassA organization commentStamp
]

{ #category : #tests }
MCClassDefinitionTest >> testDefinitionString [
	| d |
	d := self mockClassA asClassDefinition.
	self assert: d definitionString equals: self mockClassA definition.
]

{ #category : #tests }
MCClassDefinitionTest >> testEquals [
	| a b |
	a := self mockClass: 'ClassA' super: 'SuperA'.
	b := self mockClass: 'ClassA' super: 'SuperA'.
	self assert: a equals: b
]

{ #category : #tests }
MCClassDefinitionTest >> testEqualsSensitivity [
	| message a b defA args defB |
	message := self creationMessage.
	a := #(ClassA SuperA CategoryA #(iVarA) #(CVarA) #(PoolA) #(ciVarA)
			typeA 'A comment' 'A').
	b := #(ClassB SuperB CategoryB #(iVarB) #(CVarB) #(PoolB) #(ciVarB)
			typeB 'B comment' 'B').
	
	defA := message valueWithArguments: a.
	1 to: 8 do: [:index |
				args := a copy.
				args at: index put: (b at: index).
				defB := message valueWithArguments: args.
				self deny: defA = defB.]
]

{ #category : #tests }
MCClassDefinitionTest >> testKindOfSubclass [
	| classes |
	classes := {self mockClassA. String. Context. WeakArray. Float}.
	classes do: [:c | | d |
		d :=  c asClassDefinition.
		self assert: d kindOfSubclass equals: c kindOfSubclass.
	].
]

{ #category : #tests }
MCClassDefinitionTest >> testLoadAndUnload [
	| d c |
	d := self mockClass: 'MCMockClassC' super: 'Object'.
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	c := Smalltalk globals classNamed: 'MCMockClassC'.
	self assert: (c isKindOf: Class).
	self assert: c superclass equals: Object.
	self assert: c instVarNames isEmpty.
	self assert: c classVarNames isEmpty.
	self assert: c sharedPools isEmpty.
	self assert: c category equals: self mockCategoryName.
	self assert: c organization classComment equals: (self commentForClass: 'MCMockClassC').
	self assert: c organization commentStamp equals: (self commentStampForClass: 'MCMockClassC').
	d unload.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC')
]

{ #category : #tests }
MCClassDefinitionTest >> testValidTraitComposition [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: '{Trait1}'
		classTraitComposition: '{}'
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className).
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	cls := Smalltalk at: #MCMockClassC.
	self assert: (cls includesSelector: #c1).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait1)
]

{ #category : #tests }
MCClassDefinitionTest >> testValidTraitComposition2 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: 'Trait1'
		classTraitComposition: '{}'
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className).
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	cls := Smalltalk at: #MCMockClassC.
	self assert: (cls includesSelector: #c1).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait1)
]

{ #category : #tests }
MCClassDefinitionTest >> testValidTraitComposition3 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: 'Trait1 + Trait2'
		classTraitComposition: '{}'
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className).
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	cls := Smalltalk at: #MCMockClassC.
	self assert: (cls includesSelector: #c1).
	self assert: (cls includesSelector: #c2).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait1).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait2)
]

{ #category : #tests }
MCClassDefinitionTest >> testValidTraitComposition4 [
	"Related to http://code.google.com/p/pharo/issues/detail?id=2598"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: 'Trait1 - {#c1}'
		classTraitComposition: '{}'
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className).
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	cls := Smalltalk at: #MCMockClassC.
	self assert: (cls selectors includesAll: {#c}).
	self deny: (cls selectors includesAnyOf: {#c1})
]
