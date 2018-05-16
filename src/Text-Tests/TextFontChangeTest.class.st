"
SUnit tests for class TextFontChange
"
Class {
	#name : #TextFontChangeTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextFontChangeTest >> setUp [
	"create the prototypes for testing"
	super setUp.
	prototypes add: TextFontChange defaultFontChange.
	prototypes add: TextFontChange font1.
	prototypes add: TextFontChange font2.
	prototypes add: TextFontChange font3.
	prototypes add: TextFontChange font4.
	prototypes
		add: (TextFontChange fontNumber: 6) 
]

{ #category : #tests }
TextFontChangeTest >> testEquality [
	"Check that different instances of the same TextFontChange are equal"
	self assert: TextFontChange defaultFontChange equals: TextFontChange defaultFontChange.
	self assert: TextFontChange font1 equals: TextFontChange font1.
	self assert: TextFontChange font2 equals: TextFontChange font2.
	self assert: TextFontChange font3 equals: TextFontChange font3.
	self assert: TextFontChange font4 equals: TextFontChange font4.
	self assert: (TextFontChange fontNumber: 6) equals: (TextFontChange fontNumber: 6)
]

{ #category : #tests }
TextFontChangeTest >> testHash [
	"test that different instances of the same TextFontChange hash to the 
	same value"
	| hashes hash |
	hashes := OrderedCollection new.
	1
		to: 100
		do: [:i | hashes add: TextFontChange defaultFontChange hash].
	hash := hashes at: 1.
	2
		to: 100
		do: [:i | self assert: (hashes at: i) equals: hash]
]
