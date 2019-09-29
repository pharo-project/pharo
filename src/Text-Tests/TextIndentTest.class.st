Class {
	#name : #TextIndentTest,
	#superclass : #TestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #tests }
TextIndentTest >> testAmount [
	| text  |
	text := 'Hello' asText addAttribute: (TextIndent amount: 2).
	self assert: text runs first first amount equals: 2
]

{ #category : #tests }
TextIndentTest >> testDominates [
	self assert: ((TextIndent tabs: 1) dominates: (TextIndent tabs: 2)).
	self deny: ((TextIndent tabs: 1) dominates: TextEmphasis bold).
	self deny: ((TextIndent tabs: 1) dominates: TextFontChange font2).

]

{ #category : #tests }
TextIndentTest >> testTabs [
	| text  |
	"tabs: is a synonym for amount:"
	text := 'Hello' asText addAttribute: (TextIndent tabs: 4).
	self assert: text runs first first amount equals: 4
]
