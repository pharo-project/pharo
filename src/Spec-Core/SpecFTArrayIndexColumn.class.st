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
	#name : #SpecFTArrayIndexColumn,
	#superclass : #SpecFTColumn,
	#instVars : [
		'index'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #'instance creation' }
SpecFTArrayIndexColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #'instance creation' }
SpecFTArrayIndexColumn class >> index: aNumber [
	^ self new 
		index: aNumber
]

{ #category : #'instance creation' }
SpecFTArrayIndexColumn class >> index: aNumber width: aNumber2 [
	^ (self index: aNumber) 
		width: aNumber2
]

{ #category : #accessing }
SpecFTArrayIndexColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #accessing }
SpecFTArrayIndexColumn >> index [
	^ index
]

{ #category : #accessing }
SpecFTArrayIndexColumn >> index: anObject [
	index := anObject
]

{ #category : #accessing }
SpecFTArrayIndexColumn >> transform: anArray [
	^ anArray at: self index
]
