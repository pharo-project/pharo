Class {
	#name : 'ReBadMessageRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'running' }
ReBadMessageRuleTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
ReBadMessageRuleTest >> tearDown [ 

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReBadMessageRuleTest >> testRule [ 

	| critiques aClass| 
	aClass := classFactory make: [ :aBuilder | 
		aBuilder 
			tag: 'aTag';
			package: 'aPackage' ].
	aClass class compile: 'method 1 isMemberOf: Integer' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: aClass class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (aClass class >> #method) removeFromSystem ]
]
