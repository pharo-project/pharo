"
SUnit tests for class SHStyleElement
"
Class {
	#name : #SHStyleElementTest,
	#superclass : #TestCase,
	#instVars : [
		'element'
	],
	#category : #Shout-Tests-Styling
}

{ #category : #running }
SHStyleElementTest >> setUp [
	element := SHStyleElement withTokens: #(#self #super #true #false).
]

{ #category : #tests }
SHStyleElementTest >> testPrintOn [
	"
	self debug: #testPrintOn
	"
	self assert: element printString =  'a SHStyleElement tokens: #(#self #super #true #false)'

	
]

{ #category : #tests }
SHStyleElementTest >> testStyleForTable [
	"
	self debug: #testStyleForTable
	"
	| table |
	table := element styleForTable.
	self assert: table size = 4.
	self assert: (table collect: #first) = element tokens
]
