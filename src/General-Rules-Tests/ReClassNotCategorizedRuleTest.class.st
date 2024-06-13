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
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReClassNotCategorizedRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testAllClassesAreNotTagged [

	| critiques aClass anotherClass |
	aClass := classFactory make: [ :builder |
		          builder package: 'aPackage' ].
	anotherClass := classFactory make: [ :builder |
		                builder package: 'aPackage' ].
	critiques := self myCritiquesOnClass: aClass.
	self assertEmpty: critiques.
	critiques := self myCritiquesOnClass: anotherClass.
	self assertEmpty: critiques
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testRule [

	| classCategorized classNotCategorized critiques |
	classCategorized := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	classNotCategorized := classFactory make: [ :builder |
		                     builder package: 'aPackage' ].
	critiques := self myCritiquesOnClass: classNotCategorized.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testRuleNotViolated [

	| classCategorized anotherClassCategorized critiques |
	classCategorized := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	anotherClassCategorized := classFactory make: [ :builder |
		                     builder
			                     tag: 'aTag';
			                     package: 'aPackage' ].
	critiques := self myCritiquesOnClass: anotherClassCategorized.
	self assertEmpty: critiques
]
