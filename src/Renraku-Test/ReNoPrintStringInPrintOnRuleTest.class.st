Class {
	#name : #ReNoPrintStringInPrintOnRuleTest,
	#superclass : #ReCodeBasedTestCase,
	#category : #'Renraku-Test'
}

{ #category : #printing }
ReNoPrintStringInPrintOnRuleTest >> printOn: aStream [
	"I violate my rule on not using pringString inside a printOn:"
	super printOn: aStream.
	3 factorial printString.
	2 even ifFalse: [ 3.14 printString ]
]

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

{ #category : #tests }
ReNoPrintStringInPrintOnRuleTest >> testRule [
	|method critiques|
	method := self class >> #printOn:.
	critiques := OrderedCollection new.
	ReNoPrintStringInPrintOnRule new check: method forCritiquesDo:[:critique | critiques add: critique].
	
	self assert: critiques size equals: 2.
	self assert: critiques first sourceAnchor interval equals: (108 to: 130)
]
