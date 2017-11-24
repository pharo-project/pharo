Class {
	#name : #SpecFTColumn,
	#superclass : #Object,
	#instVars : [
		'id',
		'width'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #'instance creation' }
SpecFTColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #accessing }
SpecFTColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #layout }
SpecFTColumn >> acquireWidth: anOwnerWidth [
	^ self widthOrUndefined min: anOwnerWidth
]

{ #category : #accessing }
SpecFTColumn >> id [
	^ id
]

{ #category : #initialization }
SpecFTColumn >> id: anObject [
	id := anObject
]

{ #category : #accessing }
SpecFTColumn >> transform: anObject [
	^ anObject
]

{ #category : #accessing }
SpecFTColumn >> width [
	^ width
]

{ #category : #accessing }
SpecFTColumn >> width: anObject [
	width := anObject
]

{ #category : #accessing }
SpecFTColumn >> widthOrUndefined [
	^ self width ifNil: [ self class undefinedColumnWidth ]
]
