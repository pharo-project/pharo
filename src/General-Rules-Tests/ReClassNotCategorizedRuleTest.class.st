Class {
	#name : 'ReClassNotCategorizedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
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

	| classCatorized classNotCatorized critiques |
	classCatorized := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	classNotCatorized := classFactory make: [ :builder |
		                     builder package: 'aPackage' ].
	critiques := self myCritiquesOnClass: classNotCatorized.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReClassNotCategorizedRuleTest >> testRuleNotViolated [

	| classCatorized anotherClassCatorized critiques |
	classCatorized := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	anotherClassCatorized := classFactory make: [ :builder |
		                     builder
			                     tag: 'aTag';
			                     package: 'aPackage' ].
	critiques := self myCritiquesOnClass: anotherClassCatorized.
	self assertEmpty: critiques
]
