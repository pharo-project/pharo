"
SUnit tests for generating argument names
"
Class {
	#name : #ArgumentNamesTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Debugger'
}

{ #category : #asserting }
ArgumentNamesTest >> argumentNameFor: anObject shouldBe: aString [ 

	self assert: anObject class canonicalArgumentName equals: aString.
]

{ #category : #asserting }
ArgumentNamesTest >> argumentNameForInstanceOf: aClass shouldBe: aString [ 
	
	self assert: aClass canonicalArgumentName equals: aString.
]

{ #category : #tests }
ArgumentNamesTest >> testAPrefixForClassNameStartingWithConsonant [

	self testCollection.
]

{ #category : #tests }
ArgumentNamesTest >> testAnPrefixForClassNameStartingWithVowel [

	self testInteger.
]

{ #category : #tests }
ArgumentNamesTest >> testClass [

	self argumentNameForInstanceOf: Class shouldBe: 'aClass'.
	self argumentNameFor: Integer shouldBe: 'aClass'.
	self argumentNameFor: Collection shouldBe: 'aClass'.
]

{ #category : #tests }
ArgumentNamesTest >> testCollection [

	self argumentNameForInstanceOf: Collection shouldBe: 'aCollection'.
]

{ #category : #tests }
ArgumentNamesTest >> testDefault [

	self argumentNameForInstanceOf: Object shouldBe: 'anObject'.
]

{ #category : #tests }
ArgumentNamesTest >> testInteger [

	self argumentNameForInstanceOf: Integer shouldBe: 'anInteger'.
]

{ #category : #tests }
ArgumentNamesTest >> testNumber [

	self argumentNameForInstanceOf: Number shouldBe: 'aNumber'.
	self argumentNameForInstanceOf: Float shouldBe: 'aNumber'.
]

{ #category : #tests }
ArgumentNamesTest >> testString [

	self argumentNameForInstanceOf: String shouldBe: 'aString'.
	self argumentNameForInstanceOf: ByteString shouldBe: 'aString'.
]
