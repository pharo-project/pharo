Class {
	#name : 'TraitFileOutTest',
	#superclass : 'AbstractTraitsOnPreparedModelTest',
	#instVars : [
		'ca',
		'cb',
		'ta',
		'tb',
		'tc',
		'td'
	],
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'accessing' }
TraitFileOutTest class >> defaultTimeLimit [

	^ 15 seconds
]

{ #category : 'utilities' }
TraitFileOutTest >> classesAndTraitsInTestPackage [

	^ (self packageOrganizer packageNamed: self packageNameForTests) definedClasses
]

{ #category : 'running' }
TraitFileOutTest >> setUp [

	super setUp.

	td := self newTrait: #TD traits: {  }.
	td compile: 'd' classified: #cat1.
	tc := self newTrait: #TC traits: td.
	tc compile: 'c' classified: #cat1.
	tb := self newTrait: #TB traits: td.
	tb compile: 'b' classified: #cat1.
	ta := self newTrait: #TA traits: tb + tc @ { (#cc -> #c) } - { #c }.
	ta compile: 'a' classified: #cat1.

	ca := self newClass: #CA superclass: Object traits: {  }.
	ca compile: 'ca' classified: #cat1.
	cb := self newClass: #CB superclass: ca traits: ta.
	cb compile: 'cb' classified: #cat1.

	"make the class of cb also use tc:"
	cb class trait: ta classTrait + tc
]

{ #category : 'running' }
TraitFileOutTest >> tearDown [

	| dir |
	dir := FileSystem workingDirectory.
	self classesAndTraitsInTestPackage do: [ :each | (dir / each asString , 'st') ensureDelete ].
	(dir / self packageNameForTests , 'st') ensureDelete.
	super tearDown
]

{ #category : 'tests' }
TraitFileOutTest >> testFileOutCategory [
	"File out whole system category, delete all classes and traits and then
	file them in again."

	self timeLimit: 30 seconds.
	"Tempaprary hack because the old class definition builder does not manages yet classes with nil as superclass."
	{ c3 . c5 . c4 . c6 . c7 . c8 } do: [ :class | class removeFromSystem ].
	(self packageOrganizer packageNamed: self packageNameForTests)
		fileOut;
		removeFromSystem.
	self deny: (self class environment keys asSet includesAnyOf: #( #CA #CB #TA #TB #TC #TD )).

	CodeImporter evaluateFileNamed: self packageNameForTests , '.st'.
	self assertCollection: self class environment keys asSet includesAll: #( #CA #CB #TA #TB #TC #TD ).
	ta := self class environment at: #TA.
	self assert: ta traitComposition asString equals: '((TB + TC) @ {#cc->#c}) - {#c}'.
	self assertCollection: ta selectors asSet includesAll: #( #a #b #cc ).
	cb := self class environment at: #CB.
	self assert: cb traitComposition asString equals: 'TA'.
	self assertCollection: cb selectors asSet includesAll: #( #cb #a #b #cc ). "test classSide traitComposition of CB"
	self assert: cb classSide traitComposition asString equals: 'TA classTrait + TC'.
	self assertCollection: cb classSide selectors asSet includesAll: #( #d #c )
]

{ #category : 'tests' }
TraitFileOutTest >> testFileOutTrait [
	"fileOut trait T6, remove it from system and then file it in again"

	| fileName |
	self t6 compile: 'localMethod: argument ^argument'.
	self t6 classSide compile: 'localClassSideMethod: argument ^argument'.
	self t6 fileOut.
	fileName := self t6 asString , '.st'.
	self t6 removeFromSystem.

	CodeImporter evaluateFileNamed: fileName.
	self assert: (self class environment includesKey: #T6).
	self assert: self t6 traitComposition asString equals: 'T1 + (T2 @ {#m22Alias->#m22})'.
	self assertCollection: self t6 selectors asSet includesAll: #( #localMethod: #m11 #m12 #m13 #m21 #m22Alias ).
	self assert: self t6 classSide localMethods size equals: 1.
	self assertCollection: self t6 classSide selectors asSet includesAll: #( #localClassSideMethod: #m2ClassSide: )
]

{ #category : 'tests' }
TraitFileOutTest >> testRecompiling [

	| traitSrcPtr methodSrcPtr |

	traitSrcPtr := self t1 methods first sourcePointer.
	methodSrcPtr := self c1 methods first sourcePointer.

	self t1 recompile.
	self c1 recompile.

	self
		assert: traitSrcPtr equals: self t1 methods first sourcePointer;
		assert: methodSrcPtr equals: self c1 methods first sourcePointer
]

{ #category : 'tests' }
TraitFileOutTest >> testRemovingMethods [
	"When removing a local method, assure that the method
	from the trait is installed instead and that the users are
	updated."

	"Classes"
	<ignoreNotImplementedSelectors: #(m12)>
	self c2 compile: 'm12 ^0' classified: #xxx.
	self assert: (self c2 includesLocalSelector: #m12).
	self c2 removeSelector: #m12.
	self deny: (self c2 includesLocalSelector: #m12).
	self assert: (self c2 selectors includes: #m12).

	"Traits"
	self t5 compile: 'm12 ^0' classified: #xxx.
	self assert: self c2 new m12 equals: 0.
	self t5 removeSelector: #m12.
	self deny: (self t5 includesLocalSelector: #m12).
	self assert: (self t5 selectors includes: #m12).
	self assert: self c2 new m12 equals: 12
]
