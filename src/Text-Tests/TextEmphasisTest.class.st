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
TextEmphasisTest >> testDominates [
	| text attr |
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
TextEmphasisTest >> testFromCode [
	| t1 |
	t1 := TextEmphasis fromCode: 2r10101.
	self assert: t1 value equals: 2r10101
]

{ #category : #tests }
TextEmphasisTest >> testRemove [
	| t1  |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	t1 remove: TextEmphasis bold.
	self assert: t1 = TextEmphasis italic description: 'Removing bold didn''t leave italics'
]
