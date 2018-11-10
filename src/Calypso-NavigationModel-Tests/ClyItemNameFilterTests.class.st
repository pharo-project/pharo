Class {
	#name : #ClyItemNameFilterTests,
	#superclass : #ClyItemFilterTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClyItemNameFilterTests >> testComparisonWithAnotherFilter [

	| anotherFilter |
	filter := ClyItemNameFilter substringPattern: 'filter'. 
	
	anotherFilter := ClyItemNameFilter substringPattern: 'filter'. 
	self assert: filter equals: anotherFilter.
	self assert: filter hash equals: anotherFilter hash.
	
	anotherFilter := ClyItemNameFilter substringPattern: 'filter2'. 
	self deny: filter = anotherFilter.
	
]

{ #category : #tests }
ClyItemNameFilterTests >> testFilterObjectWithName [

	filter := ClyItemNameFilter substringPattern: 'ject'. "Object"
	
	self assert: (filter matches: Object).
	self deny: (filter matches: Array)
]
