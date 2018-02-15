Class {
	#name : #TraitFileOutTest,
	#superclass : #TraitsTestCase,
	#instVars : [
		'ca',
		'cb',
		'ta',
		'tb',
		'tc',
		'td'
	],
	#category : #'Tests-Traits'
}

{ #category : #running }
TraitFileOutTest >> categoryName [
	^'Traits-Tests-FileOut'
]

{ #category : #running }
TraitFileOutTest >> setUp [
	super setUp.
	self class environment organization addCategory: self categoryName.
	
	td := self createTraitNamed: #TD uses: {}.		
	td compile: 'd' classified: #cat1.
	tc := self createTraitNamed: #TC uses: td.		
	tc compile: 'c' classified: #cat1.
	tb := self createTraitNamed: #TB uses: td.		
	tb compile: 'b' classified: #cat1.
	ta := self createTraitNamed: #TA uses: tb + tc @ {#cc->#c} - {#c}.
	ta compile: 'a' classified: #cat1.
	
	ca := self createClassNamed: #CA superclass: Object uses: {}.
	ca compile: 'ca' classified: #cat1.
	cb := self createClassNamed: #CB superclass: ca uses: ta.
	cb compile: 'cb' classified: #cat1.
	
	"make the class of cb also use tc:"
	cb class uses: ta classTrait + tc instanceVariableNames: ''.
]

{ #category : #running }
TraitFileOutTest >> tearDown [
	| dir |
	dir := FileSystem workingDirectory.
	self createdClassesAndTraits, self resourceClassesAndTraits  do: [:each |
		(dir / each asString,'st') ensureDelete ] .
	(dir / self categoryName,'st') ensureDelete.
	self class environment organization removeSystemCategory: self categoryName.
	super tearDown
]

{ #category : #testing }
TraitFileOutTest >> testFileOutCategory [
	"File out whole system category, delete all classes and traits and then
	file them in again."

	| file |
	self class environment organization fileOutCategory: self categoryName.
	self class environment organization removeSystemCategory: self categoryName.
	self deny: (Smalltalk globals keys asSet includesAnyOf: #(#CA #CB #TA #TB #TC #TD)).
	[ 
	file := FileStream readOnlyFileNamed: self categoryName , '.st'.
	file fileIn ] ensure: [ file close ].
	self assertCollection: Smalltalk globals keys asSet includesAll: #(#CA #CB #TA #TB #TC #TD).
	ta := Smalltalk globals at: #TA.
	self assert: ta traitComposition asString equals: 'TB @ {#cc->#c} - {#c} + (TC @ {#cc->#c} - {#c})'.
	self assertCollection: ta selectors asSet includesAll: #(#a #b #cc).
	cb := Smalltalk globals at: #CB.
	self assert: cb traitComposition asString equals: 'TA'.
	self assertCollection: cb selectors asSet includesAll: #(#cb #a #b #cc).	"test classSide traitComposition of CB"
	self assert: cb classSide traitComposition asString equals: 'TA classTrait + TC'.
	self assertCollection: cb classSide selectors asSet includesAll: #(#d #c)
]

{ #category : #testing }
TraitFileOutTest >> testFileOutTrait [
	"fileOut trait T6, remove it from system and then file it in again"

	| fileName file |
	self t6 compile: 'localMethod: argument ^argument'.
	self t6 classSide compile: 'localClassSideMethod: argument ^argument'.
	self t6 fileOut.
	fileName := self t6 asString , '.st'.
	self resourceClassesAndTraits remove: self t6.
	self t6 removeFromSystem.
	[ 
	file := FileStream readOnlyFileNamed: fileName.
	file fileIn ] ensure: [ file close ].
	self assert: (Smalltalk globals includesKey: #T6).
	TraitsResource current t6: (Smalltalk globals at: #T6).
	self resourceClassesAndTraits add: self t6.
	self assert: self t6 traitComposition asString equals: 'T1 + (T2 @ {#m22Alias->#m22})'.
	self assertCollection: self t6 selectors asSet includesAll: #(#localMethod: #m11 #m12 #m13 #m21 #m22 #m22Alias).
	self assert: self t6 classSide methods size equals: 2.
	self assertCollection: self t6 classSide selectors asSet includesAll: #(#localClassSideMethod: #m2ClassSide:)
]

{ #category : #testing }
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

{ #category : #testing }
TraitFileOutTest >> testRemovingMethods [
	"When removing a local method, assure that the method
	from the trait is installed instead and that the users are 
	updated."

	"Classes"
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
