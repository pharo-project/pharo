"
A column for Fast table
"
Class {
	#name : #SpFTColumn,
	#superclass : #Object,
	#instVars : [
		'id',
		'width',
		'table'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #'instance creation' }
SpFTColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #accessing }
SpFTColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #layout }
SpFTColumn >> acquireWidth: anOwnerWidth [
	^ self widthOrUndefined min: anOwnerWidth
]

{ #category : #accessing }
SpFTColumn >> id [
	^ id
]

{ #category : #initialization }
SpFTColumn >> id: anObject [
	id := anObject
]

{ #category : #accessing }
SpFTColumn >> readObject: anObject [

	"Fast table presenters have a block that returns an array that is mapped by this column"
	^ self transform: (table displayBlock value: anObject)
]

{ #category : #accessing }
SpFTColumn >> table [
	^ table
]

{ #category : #accessing }
SpFTColumn >> table: anObject [
	table := anObject
]

{ #category : #accessing }
SpFTColumn >> title [
	^ id
]

{ #category : #accessing }
SpFTColumn >> transform: anObject [
	^ anObject
]

{ #category : #accessing }
SpFTColumn >> width [
	^ width
]

{ #category : #accessing }
SpFTColumn >> width: anObject [
	width := anObject
]

{ #category : #accessing }
SpFTColumn >> widthOrUndefined [
	^ self width ifNil: [ self class undefinedColumnWidth ]
]
