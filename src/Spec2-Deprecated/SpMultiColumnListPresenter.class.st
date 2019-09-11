"
=== Deprecated ===
Use TablePresenter instead.

A MultiColumnListPresenter is a spec presenter for Multi columns list


This presenter configures the columns based on the displayBlock of the first item.
The displayBlock should return an array with the values for each of the columns.
The number of elements of the array of the first element's display value is used as the number of columns.
I Assume all my elements have the same number of columns.
All columns have the same size
"
Class {
	#name : #SpMultiColumnListPresenter,
	#superclass : #SpFastTablePresenter,
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #deprecation }
SpMultiColumnListPresenter class >> isDeprecated [
	^ true
]

{ #category : #api }
SpMultiColumnListPresenter >> items: items [
	
	items ifNotEmpty: [ self numberOfColumns: (displayBlock value: items first) size ].
	super items: items
]
