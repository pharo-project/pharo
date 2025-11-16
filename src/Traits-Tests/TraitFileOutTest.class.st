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
