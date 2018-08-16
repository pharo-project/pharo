Class {
	#name : #ReNoPrintStringInPrintOnRuleTest,
	#superclass : #ReCodeBasedTestCase,
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReNoPrintStringInPrintOnRuleTest >> testBasicCheck [
	
	testMethod := testClass >> (testClass compile: 'printOn: aStream ^ self printString').
	
	self
		assert: (testMethod critiques anySatisfy: [ :critic | critic rule class = ReNoPrintStringInPrintOnRule ]).
	
	testMethod := testClass >> (testClass compile: 'printOn: aStream ^ aStream nextPutAll: ''Toto'' ').
	
	self
		assert: (testMethod critiques noneSatisfy: [ :critic | critic rule class = ReNoPrintStringInPrintOnRule ])
]
