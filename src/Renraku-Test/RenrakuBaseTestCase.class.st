"
I provide a testing package which is destroyed later
"
Class {
	#name : #RenrakuBaseTestCase,
	#superclass : #TestCase,
	#instVars : [
		'testPackage'
	],
	#classVars : [
		'ScreamerRule'
	],
	#category : #Renraku-Test
}

{ #category : #running }
RenrakuBaseTestCase >> setUp [
	super setUp.
	testPackage := (RPackage named: #'Renraku-Nuclear-Site') register.
	self setupScreamerRule.
]

{ #category : #running }
RenrakuBaseTestCase >> setupScreamerRule [

	ScreamerRule := ReAbstractRule
		subclass: #ReScreamerRuleForTesting
		instanceVariableNames: ''
		classVariableNames: ''
		package: testPackage name.
		
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

{ #category : #running }
RenrakuBaseTestCase >> tearDown [

	self tearDownscreamerRule.
	testPackage unregister.
	testPackage := nil.	
	super tearDown 
]

{ #category : #running }
RenrakuBaseTestCase >> tearDownscreamerRule [

	ScreamerRule removeFromSystem.
	ScreamerRule := nil.
	ReRuleManager reset
]
