Class {
	#name : 'ReCollectionMessagesToExternalObjectRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'running' }
ReCollectionMessagesToExternalObjectRuleTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReCollectionMessagesToExternalObjectRuleTest >> tearDown [ 

	classFactory cleanUp.
	super tearDown 
]

{ #category : 'tests' }
ReCollectionMessagesToExternalObjectRuleTest >> testRule [

	| critiques aClass |
	aClass := classFactory make: [ :aBuilder | 
		aBuilder 
			name: 'ClassToSupp' ;
			slots: {#aCol  };
			tag: 'aTag';
			package: 'aPackage'
			].
	aClass class compile: 'initialize super initialize. aCol := OrderedCollection new.' classified: 'initialization'.
	aClass compile: 'aCol ^ aCol' classified: 'accessing'.
	self class compile: 'method | aClass | aClass := ClassToSupp new. aClass aCol add: 1; add: 2' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #method) removeFromSystem ]
	
]

{ #category : 'tests' }
ReCollectionMessagesToExternalObjectRuleTest >> testRuleNotViolated [

	| critiques aClass |
	aClass := classFactory make: [ :aBuilder | 
		aBuilder 
			name: 'ClassForTest';
			slots: { #aCol };
			tag: 'aTag';
			package: 'aPackage' ].
	aClass class compile: 'adding aCol add: 1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: aClass class >> #adding.
	self assertEmpty: critiques  ] ensure: [ aClass class >> #adding ]
	
	
]
