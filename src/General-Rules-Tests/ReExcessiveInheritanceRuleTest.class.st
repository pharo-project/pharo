Class {
	#name : 'ReExcessiveInheritanceRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'running' }
ReExcessiveInheritanceRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReExcessiveInheritanceRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReExcessiveInheritanceRuleTest >> testRule [
	"the superclass of ClassFactoryForTestCase is Object so the inheritance depth start at 2"
	| critiques aClass subClass1 subClass2 subClass3 subClass4 subClass5 subClass6 subClass7 myClass |
	aClass := classFactory make: [ :builder | builder ].
	subClass1 := classFactory make: [ :builder |
		             builder superclass: aClass ].
	subClass2 := classFactory make: [ :builder |
		             builder superclass: subClass1 ].
	subClass3 := classFactory make: [ :builder |
		             builder superclass: subClass2 ].
	subClass4 := classFactory make: [ :builder |
		             builder superclass: subClass3 ].
	subClass5 := classFactory make: [ :builder |
		             builder superclass: subClass4 ].
	subClass6 := classFactory make: [ :builder |
		             builder superclass: subClass5 ].
	subClass7 := classFactory make: [ :builder |
		             builder superclass: subClass6 ].
	myClass := classFactory make: [ :builder |
		           builder superclass: subClass7 ].
	critiques := self myCritiquesOnClass: myClass.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReExcessiveInheritanceRuleTest >> testRuleNotViolated [

	| critiques aClass subClass1 subClass2 subClass3 subClass4 subClass5 subClass6 myClass |
	aClass := classFactory make: [ :builder | builder ].
	subClass1 := classFactory make: [ :builder |
		             builder superclass: aClass ].
	subClass2 := classFactory make: [ :builder |
		             builder superclass: subClass1 ].
	subClass3 := classFactory make: [ :builder |
		             builder superclass: subClass2 ].
	subClass4 := classFactory make: [ :builder |
		             builder superclass: subClass3 ].
	subClass5 := classFactory make: [ :builder |
		             builder superclass: subClass4 ].
	subClass6 := classFactory make: [ :builder |
		             builder superclass: subClass5 ].
	myClass := classFactory make: [ :builder |
		           builder superclass: subClass6 ].
	critiques := self myCritiquesOnClass: myClass.
	self assertEmpty: critiques
]
