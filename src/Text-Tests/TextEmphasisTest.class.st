"
SUnit tests for TextEmphasis
"
Class {
	#name : #TextEmphasisTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextEmphasisTest >> setUp [
	super setUp.
	prototypes add: TextEmphasis bold;
		 add: TextEmphasis italic;
		 add: TextEmphasis narrow;
		 add: TextEmphasis normal;
		 add: TextEmphasis struckOut;
		 add: TextEmphasis underlined 
]

{ #category : #tests }
TextEmphasisTest >> testAdd [
	| t1 |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	self assert: (t1 = 2r11) description: 'bold and italics didn''t add correctly'
]

{ #category : #tests }
TextEmphasisTest >> testBold [
	self assert: TextEmphasis bold equals: 1.

]

{ #category : #test }
TextEmphasisTest >> testDominatedByCmd0 [
	self assert: TextEmphasis new dominatedByCmd0
]

{ #category : #tests }
TextEmphasisTest >> testDominates [
	| text |
	"self domination"
	#(#bold #italic  #underlined #narrow #struckOut) do: [ :anEmphasis |
		self assert: ((TextEmphasis perform: anEmphasis) dominates: (TextEmphasis perform: anEmphasis)) ].
	"different emphasis can be mixed, no dominant attribute"
	#(#bold #italic  #underlined #narrow #struckOut) 
		with: #(#struckOut #underlined  #narrow #italic #bold) 
		do: [:emph1 :emph2 | self deny: ((TextEmphasis perform: emph1) dominates: (TextEmphasis perform: emph2)) ].
	"In action"
	text := 'Pharo is cool' asText.
	text addAttribute: TextEmphasis bold.
	"Only one bold at time"
	text addAttribute: TextEmphasis bold.
	self assert: (text attributesAt: 1) size equals: 1.
	self assert: (text attributesAt: 1) first emphasisCode equals: 1.
	"but can be both bold and underlined"
	text addAttribute: TextEmphasis underlined.
	self assert: (text attributesAt: 1) size equals: 2.
	self assert: (text attributesAt: 1) first emphasisCode equals: 1.
	self assert: (text attributesAt: 1) second emphasisCode equals: 4.
	"and even in italic"
	text addAttribute: TextEmphasis italic.
	self assert: (text attributesAt: 1) size equals: 3.
	self assert: (text attributesAt: 1) third emphasisCode equals: 2.
]

{ #category : #tests }
TextEmphasisTest >> testEmphasisCode [
	| attr |
	"test each emphasis code value"
	self assert: TextEmphasis normal emphasisCode equals: 0.
	self assert: TextEmphasis bold emphasisCode equals: 1.
	self assert: TextEmphasis italic emphasisCode equals: 2.
	self assert: TextEmphasis underlined emphasisCode equals: 4.
	self assert: TextEmphasis narrow emphasisCode equals: 8.
	self assert: TextEmphasis struckOut emphasisCode equals: 16.
	"Turn bold to underlined"
	attr := TextEmphasis bold emphasisCode: 4.
	self assert: attr equals: 4.
	"We can add the emphasisCode"
	attr := TextEmphasis new emphasisCode: 
		TextEmphasis italic emphasisCode 
		+ TextEmphasis bold emphasisCode 
		+ TextEmphasis struckOut  emphasisCode.
	self assert: attr emphasisCode equals: 2r10011
]

{ #category : #tests }
TextEmphasisTest >> testFromCode [
	| t1 |
	t1 := TextEmphasis fromCode: 2r10101.
	self assert: t1 value equals: 2r10101
]

{ #category : #tests }
TextEmphasisTest >> testItalic [
	self assert: TextEmphasis italic equals: 2

]

{ #category : #tests }
TextEmphasisTest >> testNarrow [
	self assert: TextEmphasis narrow  equals: 8

]

{ #category : #tests }
TextEmphasisTest >> testNormal [
	self assert: TextEmphasis normal equals: 0

]

{ #category : #tests }
TextEmphasisTest >> testRemove [
	| t1  |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	t1 remove: TextEmphasis bold.
	self assert: t1 = TextEmphasis italic description: 'Removing bold didn''t leave italics'
]

{ #category : #tests }
TextEmphasisTest >> testSet [
	"Not set with normal emphasis..."
	self deny: TextEmphasis normal set.
	"...but set for the other emphasis"
	#(#bold #italic #underlined #narrow #struckOut) do: [ :anEmphasis |
		self assert: (TextEmphasis perform: anEmphasis) set].
	"and can be turned off"
	#(#bold #italic #underlined #narrow #struckOut) do: [ :anEmphasis |
		| attr |
		attr := TextEmphasis perform: anEmphasis.
		attr turnOff.
		self deny: attr set].
]

{ #category : #tests }
TextEmphasisTest >> testStruckOut [
	self assert: TextEmphasis struckOut equals: 16

]

{ #category : #tests }
TextEmphasisTest >> testUnderlined [
	self assert: TextEmphasis underlined equals: 4

]

{ #category : #tests }
TextEmphasisTest >> testValue [
	| attr |
	"asking for the value return the emphasis code"
	self assert: TextEmphasis normal value equals: 0.
	self assert: TextEmphasis bold value equals: 1.
	self assert: TextEmphasis italic value equals: 2.
	self assert: TextEmphasis underlined value equals: 4.
	self assert: TextEmphasis narrow value equals: 8.
	self assert: TextEmphasis struckOut value equals: 16.

	"We can add the emphasisCode, value should respond accordingly"
	attr := TextEmphasis new emphasisCode: 
		TextEmphasis italic emphasisCode 
		+ TextEmphasis bold emphasisCode 
		+ TextEmphasis struckOut  emphasisCode.
	self assert: attr value equals: 2r10011.
	"equality is based on the value"
	self assert: attr equals: 2r10011
]
