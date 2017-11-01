"
Like SortHierarchically but sort by name and without indentation.
"
Class {
	#name : #SortAlphabeticallyClassList,
	#superclass : #Object,
	#instVars : [
		'list'
	],
	#category : 'Nautilus'
}

{ #category : #'instance creation' }
SortAlphabeticallyClassList class >> buildHierarchyForClasses: aCollection [
	| nodes |
	nodes := aCollection asOrderedCollection sort: [:a :b  | a name < b name].
	^ self new
		list: nodes;
		yourself
]

{ #category : #'instance creation' }
SortAlphabeticallyClassList class >> buildListFor: aPackageGroup in: anEnvironment [
	^ aPackageGroup withClassesIn: anEnvironment createHierarchySorter: self 
]

{ #category : #combining }
SortAlphabeticallyClassList >> , another [ 

	^ self class new
		list: (self list, another list);
		yourself
]

{ #category : #initialization }
SortAlphabeticallyClassList >> initialize [
	list := OrderedCollection new.
]

{ #category : #accessing }
SortAlphabeticallyClassList >> list [
	^ list
]

{ #category : #accessing }
SortAlphabeticallyClassList >> list: aList [
	list := aList
]

{ #category : #accessing }
SortAlphabeticallyClassList >> sortedElements [
	^ list
]
