Class {
	#name : #ClySortSystemItemFunctionTests,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #tests }
ClySortSystemItemFunctionTests >> testCompareClassesAndMethods [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Object )
]

{ #category : #tests }
ClySortSystemItemFunctionTests >> testCompareTwoClasses [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object value: Array).
	self assert: (function value: Array value: Object).
	self assert: (function value: Object value: Object).
]

{ #category : #tests }
ClySortSystemItemFunctionTests >> testCompareTwoMethods [

	function := ClySortSystemItemFunction ascending.
	
	self deny: (function value: Object >> #size value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Object >> #size ).
	self assert: (function value: Object >> #printString value: Object >> #printString )
]
