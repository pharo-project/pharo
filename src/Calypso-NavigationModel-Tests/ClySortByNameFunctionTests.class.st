Class {
	#name : #ClySortByNameFunctionTests,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClySortByNameFunctionTests >> testAscending [

	function := ClySortByNameFunction ascending.
	
	self deny: (function value: Object value: Array ).
	self assert: (function value: Array value: Object ).
	self assert: (function value: Object value: Object )
]

{ #category : #tests }
ClySortByNameFunctionTests >> testDescending [

	function := ClySortByNameFunction descending.
	
	self assert: (function value: Object value: Array ).
	self deny: (function value: Array value: Object ).
	self assert: (function value: Object value: Object )
]
