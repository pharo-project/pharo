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
	#name : #FTArrayIndexColumn,
	#superclass : #Object,
	#instVars : [
		'id',
		'width',
		'index'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #'instance creation' }
FTArrayIndexColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #'instance creation' }
FTArrayIndexColumn class >> index: aNumber [
	^ self new 
		index: aNumber
]

{ #category : #'instance creation' }
FTArrayIndexColumn class >> index: aNumber width: aNumber2 [
	^ (self index: aNumber) 
		width: aNumber2
]

{ #category : #accessing }
FTArrayIndexColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #layout }
FTArrayIndexColumn >> acquireWidth: anOwnerWidth [
	^ self widthOrUndefined min: anOwnerWidth
]

{ #category : #accessing }
FTArrayIndexColumn >> id [
	^ id
]

{ #category : #accessing }
FTArrayIndexColumn >> id: anObject [
	id := anObject
]

{ #category : #accessing }
FTArrayIndexColumn >> index [
	^ index
]

{ #category : #accessing }
FTArrayIndexColumn >> index: anObject [
	index := anObject
]

{ #category : #accessing }
FTArrayIndexColumn >> transform: anArray [
	^ anArray at: self index
]

{ #category : #accessing }
FTArrayIndexColumn >> width [
	^ width
]

{ #category : #accessing }
FTArrayIndexColumn >> width: anObject [
	width := anObject
]

{ #category : #accessing }
FTArrayIndexColumn >> widthOrUndefined [
	^ self width ifNil: [ self class undefinedColumnWidth ]
]
