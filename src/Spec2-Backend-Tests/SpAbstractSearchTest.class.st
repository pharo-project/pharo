Class {
	#name : #SpAbstractSearchTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractSearchTest class >> isAbstract [

	^ self = SpAbstractSearchTest
]

{ #category : #running }
SpAbstractSearchTest >> initializeTestedInstance [ 

	presenter items: #(10 20 30).
]

{ #category : #accessing }
SpAbstractSearchTest >> selectIndex: index [

	self presenter selection selectIndex: index	
]

{ #category : #accessing }
SpAbstractSearchTest >> selectedIndex [

	^ self presenter selection selectedIndex
]

{ #category : #tests }
SpAbstractSearchTest >> testSearchWithFunction [

	self presenter searchMatching: [ :item :textToSearch | 
		(item - 1) asString beginsWith: textToSearch ].
	self selectIndex: 1. "10"
	self adapter typeForSearch: '2'.
	self deny: self selectedIndex equals: 2.
	self adapter typeForSearch: '2'. 
	self assert: self selectedIndex equals: 3 "20"
]

{ #category : #tests }
SpAbstractSearchTest >> testTypePerformsSearch [

	self selectIndex: 1. "10"
	self adapter typeForSearch: '2'. 
	self assert: self selectedIndex equals: 2 "20"
]
