"
I am an abstract class that describe a Strategy to search/filter a FTTreeDataSource.

Description
----------------------

I have a dataSource that I can use and I can have a seach pattern which I use to search/filter the dataSource if needed.

I am use by a FTTreeDataSource and I help it with the possible FTFunction of his FTTableMorph. 

Public API and Key Messages
----------------------

- #search    	activate a search of the dataSource with the current pattern.
	
- #filter    	activate the filter of the dataSource with the current pattern.
	
- #searchWith: aString dataSource: dataSource 	is one of my constructors, it return the result of a search.

-#filterWith: aRegex dataSource: dataSource 	is an other of my constructor, it returns a new dataSource filtered by the regex. 
	
Example
----------------------

I am an abstract class. I should not be use.
 
Internal Representation and Key Implementation Points.
----------------------

    Instance Variables
	dataSource:		The data source I need to search/filter.
	pattern:		The search pattern in need to use,  this is a String or a Regex.

"
Class {
	#name : #FTTreeFunctionStrategy,
	#superclass : #Object,
	#instVars : [
		'dataSource',
		'pattern'
	],
	#category : #'Morphic-Widgets-FastTable-Tree'
}

{ #category : #'instance-creation' }
FTTreeFunctionStrategy class >> filterWith: aRegex dataSource: aDataSource [
	^ self new
		pattern: aRegex;
		dataSource: aDataSource;
		filter
]

{ #category : #'instance-creation' }
FTTreeFunctionStrategy class >> searchWith: aString dataSource: aDataSource [
	^ self new pattern: aString; dataSource: aDataSource; search
]

{ #category : #accessing }
FTTreeFunctionStrategy >> dataSource [
	^ dataSource
]

{ #category : #accessing }
FTTreeFunctionStrategy >> dataSource: anObject [
	dataSource := anObject
]

{ #category : #action }
FTTreeFunctionStrategy >> filter [
	"I need to return a new dataSource that filter my dataSource."

	self subclassResponsibility
]

{ #category : #accessing }
FTTreeFunctionStrategy >> isMatching: anItem [
	^ (dataSource toString: anItem) beginsWith: pattern
]

{ #category : #action }
FTTreeFunctionStrategy >> optionalOperations [
	^ pattern isEmptyOrNil
		ifTrue: [ #() ]
		ifFalse: [ nil ]
]

{ #category : #accessing }
FTTreeFunctionStrategy >> pattern: aStringOrRegex [
	pattern := aStringOrRegex
]

{ #category : #action }
FTTreeFunctionStrategy >> realSearch [
	"I need to return a collection of index that match the elements I find."

	self subclassResponsibility
]

{ #category : #action }
FTTreeFunctionStrategy >> search [
	^ self optionalOperations 
		ifNotNil: [ :res | res ]
		ifNil: [ self realSearch]
]
