Class {
	#name : #ClySortByNameFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClySortByNameFunctionTest >> testAscending [

	function := ClySortByNameFunction ascending.
	
	self deny: (function value: Object value: Array ).
	self assert: (function value: Array value: Object ).
	self assert: (function value: Object value: Object )
]

{ #category : #tests }
ClySortByNameFunctionTest >> testDescending [

	function := ClySortByNameFunction descending.
	
	self assert: (function value: Object value: Array ).
	self deny: (function value: Array value: Object ).
	self assert: (function value: Object value: Object )
]
