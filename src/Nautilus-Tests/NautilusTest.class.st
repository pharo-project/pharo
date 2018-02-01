"
A NautilusTest is a test class for testing the behavior of Nautilus
"
Class {
	#name : #NautilusTest,
	#superclass : #TestCase,
	#instVars : [
		'nautilus'
	],
	#category : #'Nautilus-Tests'
}

{ #category : #running }
NautilusTest >> setUp [
	super setUp.
	nautilus := Nautilus new.
]

{ #category : #running }
NautilusTest >> tearDown [

	nautilus := nil.
	super tearDown
]

{ #category : #tests }
NautilusTest >> testSelectedCategory [

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	nautilus selectedCategory: 'test'.
	self assert: (nautilus selectedCategory = 'test').
	self assert: (nautilus selectedMethod isNil).
]

{ #category : #tests }
NautilusTest >> testSelectedClass [

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	nautilus selectedCategory: 'test'.
	self assert: (nautilus selectedCategory = 'test').
	self assert: (nautilus selectedMethod isNil).
	
	nautilus selectedClass: Morph.
	self assert: (nautilus selectedClass = Morph).
	self assert: (nautilus selectedCategory isNil).
	self assert: (nautilus selectedMethod isNil).
]

{ #category : #tests }
NautilusTest >> testSelectedMethod [

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	self assert: (nautilus selectedMethod = (Object>>#at:))
]
