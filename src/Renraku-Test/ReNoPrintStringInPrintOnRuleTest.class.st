Class {
	#name : #ReNoPrintStringInPrintOnRuleTest,
	#superclass : #ReCodeBasedTestCase,
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReNoPrintStringInPrintOnRuleTest >> testBasicCheck [
	
	testMethod := testClass >> (testClass compile: 
	'printOn: aStream 
		| anObject | 
		anObject := { ''one'', ''two'' } asOrderedCollection. 
		^ aStream nextPutAll: anObject printString.' ).
	
	self
		assert: (testMethod critiques anySatisfy: [ :critic | critic rule class = ReNoPrintStringInPrintOnRule ]).
	
	testMethod := testClass >> (testClass compile: 
	'printOn: aStream 
		| anObject | 
		anObject := { ''one'', ''two'' } asOrderedCollection. 
		^ aStream print anObject.').
	
	self
		assert: (testMethod critiques noneSatisfy: [ :critic | critic rule class = ReNoPrintStringInPrintOnRule ])
]
