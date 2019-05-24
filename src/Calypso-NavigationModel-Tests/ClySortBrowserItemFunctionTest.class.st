Class {
	#name : #ClySortBrowserItemFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClySortBrowserItemFunctionTest >> testCompareTwoItemsOfClasses [

	function := ClySortBrowserItemFunction with: ClySortByNameFunction ascending.
	
	self deny: (function value: Object asCalypsoBrowserItem value: Array asCalypsoBrowserItem).
	self assert: (function value: Array asCalypsoBrowserItem value: Object asCalypsoBrowserItem).
	self assert: (function value: Object asCalypsoBrowserItem value: Object asCalypsoBrowserItem).
]

{ #category : #tests }
ClySortBrowserItemFunctionTest >> testComparisonWithAnotherFunction [

	| function2 |
	function := ClySortBrowserItemFunction with: ClySortByNameFunction ascending.
	function2 := ClySortBrowserItemFunction with: ClySortByNameFunction ascending.
	
	self assert: function equals: function2.
	self assert: function hash equals: function2 hash.
	
	function2 := ClySortBrowserItemFunction with: ClySortByNameFunction descending.
	self deny: function = function2.
]
