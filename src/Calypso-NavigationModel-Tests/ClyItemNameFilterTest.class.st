Class {
	#name : 'ClyItemNameFilterTest',
	#superclass : 'ClyItemFilterTest',
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'tests' }
ClyItemNameFilterTest >> testComparisonWithAnotherFilter [
	| anotherFilter |
	filter := ClyItemNameFilter substringPattern: 'filter'.

	anotherFilter := ClyItemNameFilter substringPattern: 'filter'.
	self assert: filter equals: anotherFilter.
	self assert: filter hash equals: anotherFilter hash.

	anotherFilter := ClyItemNameFilter substringPattern: 'filter2'.
	self deny: filter equals: anotherFilter
]

{ #category : 'tests' }
ClyItemNameFilterTest >> testFilterClassWithName [

	filter := ClyItemNameFilter substringPattern: 'ject'. "Object"

	self assert: (filter matches: Object).
	self deny: (filter matches: Array)
]

{ #category : 'tests' }
ClyItemNameFilterTest >> testFilterMethodWithSelector [

	filter := ClyItemNameFilter substringPattern: 'collect'.

	self assert: (filter matches: Collection >> #collect:).
	self deny: (filter matches: Collection >> #add:)
]

{ #category : 'tests' }
ClyItemNameFilterTest >> testFilterPackageWithName [

	filter := ClyItemNameFilter substringPattern: 'kernel'.

	self assert: (filter matches: (self packageOrganizer packageNamed: #Kernel)).
	self deny: (filter matches: (self packageOrganizer packageNamed: #Jobs))
]
