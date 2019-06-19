Class {
	#name : #ClySortSystemItemFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #tests }
ClySortSystemItemFunctionTest >> testCompareClassesAndMethods [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Object )
]

{ #category : #tests }
ClySortSystemItemFunctionTest >> testCompareTwoClasses [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object value: Array).
	self assert: (function value: Array value: Object).
	self assert: (function value: Object value: Object).
]

{ #category : #tests }
ClySortSystemItemFunctionTest >> testCompareTwoMethods [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object >> #size value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Object >> #size ).
	self assert: (function value: Object >> #printString value: Object >> #printString )
]
