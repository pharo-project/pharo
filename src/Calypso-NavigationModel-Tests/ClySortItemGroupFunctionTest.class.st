Class {
	#name : #ClySortItemGroupFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClySortItemGroupFunctionTest >> testAscending [

	function := ClySortItemGroupFunction ascending.
 
	self assert: (function 
		value: (ClyItemGroup named: 'group1' priority: 20) 
		value: (ClyItemGroup named: 'group2' priority: 10) ).
	self deny: (function 
		value: (ClyItemGroup named: 'group1' priority: 10) 
		value: (ClyItemGroup named: 'group2' priority: 20)).	
	self deny: (function 
		value: (ClyItemGroup named: 'group2') value: (ClyItemGroup named: 'group1') ).
	self assert: (function 
		value: (ClyItemGroup named: 'group1') value: (ClyItemGroup named: 'group2'))
]

{ #category : #tests }
ClySortItemGroupFunctionTest >> testDescending [

	function := ClySortItemGroupFunction descending.
 
	self deny: (function 
		value: (ClyItemGroup named: 'group1' priority: 20) 
		value: (ClyItemGroup named: 'group2' priority: 10) ).
	self assert: (function 
		value: (ClyItemGroup named: 'group1' priority: 10) 
		value: (ClyItemGroup named: 'group2' priority: 20)).	
	self assert: (function 
		value: (ClyItemGroup named: 'group2') value: (ClyItemGroup named: 'group1') ).
	self deny: (function 
		value: (ClyItemGroup named: 'group1') value: (ClyItemGroup named: 'group2'))
]
