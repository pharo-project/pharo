"
I am a fast table column to represent item in array at particular index.
 
Internal Representation and Key Implementation Points.

This class has and extends API of FTColumn

    Instance Variables
	id:			An id for the column
	width:		An Integer that represent the width of a column.
	index:		<Number>
"
Class {
	#name : #SpFTArrayIndexColumn,
	#superclass : #SpFTColumn,
	#instVars : [
		'index'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #'instance creation' }
SpFTArrayIndexColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #'instance creation' }
SpFTArrayIndexColumn class >> index: aNumber [
	^ self new 
		index: aNumber
]

{ #category : #'instance creation' }
SpFTArrayIndexColumn class >> index: aNumber width: aNumber2 [
	^ (self index: aNumber) 
		width: aNumber2
]

{ #category : #accessing }
SpFTArrayIndexColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #accessing }
SpFTArrayIndexColumn >> index [
	^ index
]

{ #category : #accessing }
SpFTArrayIndexColumn >> index: anObject [
	index := anObject
]

{ #category : #accessing }
SpFTArrayIndexColumn >> transform: anArray [
	^ anArray at: self index
]
