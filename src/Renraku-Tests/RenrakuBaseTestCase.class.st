"
I provide a testing package which is destroyed later
"
Class {
	#name : 'RenrakuBaseTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'testPackage'
	],
	#classVars : [
		'ScreamerRule'
	],
	#category : 'Renraku-Tests',
	#package : 'Renraku-Tests'
}

{ #category : 'testing' }
RenrakuBaseTestCase class >> isAbstract [

	^ self = RenrakuBaseTestCase
]

{ #category : 'running' }
RenrakuBaseTestCase >> setUp [

	super setUp.
	testPackage := self packageOrganizer ensurePackage: #RenrakuCreatedForTests.
	self setupScreamerRule
]

{ #category : 'running' }
RenrakuBaseTestCase >> setupScreamerRule [

	ScreamerRule := self class classInstaller make: [ :aClassBuilder |
		aClassBuilder
			name: #ReScreamerRuleForTesting;
			superclass: ReAbstractRule;
			package: testPackage name ].


	ScreamerRule class
		compile: 'checksMethod ^ true';
		compile: 'checksClass ^ true';
		compile: 'checksPackage ^ true'.

	ScreamerRule
		compile: 'name ^ ''A tautological violation for testing''';
		compile: 'check: aNode forCritiquesDo: aBlock
						aBlock cull: (self critiqueFor: aNode)'.

	ReRuleManager reset
]

{ #category : 'running' }
RenrakuBaseTestCase >> tearDown [

	self tearDownscreamerRule.
	testPackage removeFromSystem.
	super tearDown
]

{ #category : 'running' }
RenrakuBaseTestCase >> tearDownscreamerRule [

	ScreamerRule removeFromSystem.
	ScreamerRule := nil.
	ReRuleManager reset
]
