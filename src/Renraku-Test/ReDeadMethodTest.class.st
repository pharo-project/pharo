Class {
	#name : #ReDeadMethodTest,
	#superclass : #ReCodeBasedTestCase,
	#instVars : [
		'validTestPackage'
	],
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReDeadMethodTest >> testBasicCheck [
	
	testMethod := testClass >> (testClass compile: 'toto Object new toto2').
	
	self
		assert: (testMethod critiques anySatisfy: [ :critic | critic rule class = ReDeadMethod ]).
	
	testMethod := testClass >> (testClass compile: 'printOn: aStream ^ aStream nextPutAll: ''Toto'' ').
	
	self
		assert: (testMethod critiques noneSatisfy: [ :critic | critic rule class = ReDeadMethod ]).
		
]
