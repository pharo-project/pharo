"
Example class for ClassTest
"
Class {
	#name : 'ObsoleteTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'ReleaseTests-CleanCode',
	#package : 'ReleaseTests',
	#tag : 'CleanCode'
}

{ #category : 'accessing' }
ObsoleteTest class >> defaultTimeLimit [

	^ 30 seconds
]

{ #category : 'running' }
ObsoleteTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ObsoleteTest >> tearDown [

	classFactory cleanUp.
	Smalltalk fixObsoleteReferences.
	super tearDown
]

{ #category : 'tests' }
ObsoleteTest >> testClassObsolete [

	| class name |
	class := classFactory newClass.
	name := class name.
	self deny: class isObsolete.
	self deny: class class isObsolete.
	class removeFromSystem.
	self assert: class isObsolete.
	self assert: class class isObsolete.
	self assert: (class originalName beginsWith: name)
]

{ #category : 'tests' }
ObsoleteTest >> testFixObsoleteSharedPools [

	| poolClass obsoletePoolName testClass preFixObsoleteClassNames postFixObsoleteClassNames |
	poolClass := classFactory newClass. "provides unique name over time via class variable counter"

	testClass := classFactory make: [ :aBuilder | aBuilder sharedPools: { poolClass } ].

	classFactory delete: poolClass.
	obsoletePoolName := poolClass name.
	poolClass := nil.
	3 timesRepeat: [ Smalltalk garbageCollect ].

	preFixObsoleteClassNames := SystemNavigation default obsoleteClasses collect: [ :each | each name ].
	self assert: (preFixObsoleteClassNames includes: obsoletePoolName).
	self assert: testClass sharedPoolNames size equals: 1.
	self assert: (testClass sharedPoolNames includes: obsoletePoolName).
	self assert: (testClass sharedPoolsString beginsWith: 'AnObsolete').

	Smalltalk fixObsoleteSharedPools.

	postFixObsoleteClassNames := SystemNavigation default obsoleteClasses collect: #name.
	self deny: (postFixObsoleteClassNames includes: obsoletePoolName).
	self assertEmpty: testClass sharedPoolNames.
	self assertEmpty: testClass sharedPoolsString
]

{ #category : 'tests' }
ObsoleteTest >> testTraitObsolete [

	| aClass aTrait |
	aTrait := classFactory newTrait.
	aClass := classFactory make: [ :aBuilder | aBuilder traitComposition: aTrait ].

	self deny: aTrait isObsolete.
	aTrait removeFromSystem.
	self assert: aTrait isObsolete.
	self deny: aClass isObsolete
]
