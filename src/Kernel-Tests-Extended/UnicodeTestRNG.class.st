"
The random number generator used by the UnicodeTest
"
Class {
	#name : #UnicodeTestRNG,
	#superclass : #TestResource,
	#instVars : [
		'generator'
	],
	#category : #'Kernel-Tests-Extended-Charset'
}

{ #category : #private }
UnicodeTestRNG >> generator [
	^ generator
]

{ #category : #private }
UnicodeTestRNG >> next [
	^ generator next
]

{ #category : #running }
UnicodeTestRNG >> randomCharacter [
	^ self randomCharacterBetween: 0 and: Unicode maxValue  
]

{ #category : #running }
UnicodeTestRNG >> randomCharacterAtOrAbove: lower [
	^ self randomCharacterBetween: lower and: Unicode maxValue  
]

{ #category : #running }
UnicodeTestRNG >> randomCharacterBetween: lower and: upper [
	^ Character codePoint: (self randomCodePointBetween: lower and: upper) 
]

{ #category : #running }
UnicodeTestRNG >> randomCodePoint [
	^ self randomCodePointBetween: 0 and: Unicode maxValue  
]

{ #category : #running }
UnicodeTestRNG >> randomCodePointAtOrAbove: lower [
	^ self randomCodePointBetween: lower and: Unicode maxValue  
]

{ #category : #running }
UnicodeTestRNG >> randomCodePointBetween: lower and: upper [
	| max span codePoint |
	max := upper min: 16rE01EF.
	span := max - lower + 1.
	[
		codePoint := (self generator next * span) floor + lower.
		Unicode isNonCharacter: (Character codePoint: codePoint) 
	] whileTrue.
	^ codePoint
]

{ #category : #running }
UnicodeTestRNG >> setUp [
	super setUp.
	generator := Random seed: 14159265.
]
