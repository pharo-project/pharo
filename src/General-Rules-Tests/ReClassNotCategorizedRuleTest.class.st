Class {
	#name : 'ReClassNotCategorizedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'running' }
ReClassNotCategorizedRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new
]

{ #category : 'running' }
ReClassNotCategorizedRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testAllClassesAreNotTagged [

	| aClass anotherClass |
	aClass := classFactory make: [ :builder | builder package: 'APackage' ].
	anotherClass := classFactory make: [ :builder | builder package: 'APackage' ].

	self assertEmpty: (self myCritiquesOnClass: aClass).
	self assertEmpty: (self myCritiquesOnClass: anotherClass)
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testRule [

	| classCategorized classNotCategorized |
	classCategorized := classFactory make: [ :builder |
		                    builder
			                    tag: 'aTag';
			                    package: 'APackage' ].
	classNotCategorized := classFactory make: [ :builder | builder package: 'APackage' ].
	self assert: (self myCritiquesOnClass: classNotCategorized) size equals: 1
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testRuleNotViolated [

	| classCategorized anotherClassCategorized |
	classCategorized := classFactory make: [ :builder |
		                    builder
			                    tag: 'aTag';
			                    package: 'APackage' ].
	anotherClassCategorized := classFactory make: [ :builder |
		                           builder
			                           tag: 'aTag';
			                           package: 'APackage' ].
	self assertEmpty: (self myCritiquesOnClass: anotherClassCategorized)
]
