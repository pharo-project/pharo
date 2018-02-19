"
I am an error raised when extending a class with an incompatible layout.
"
Class {
	#name : #IncompatibleLayoutConflict,
	#superclass : #ClassBuilderError,
	#instVars : [
		'layout',
		'subType'
	],
	#category : #'Slot-Core-Exception'
}

{ #category : #accessing }
IncompatibleLayoutConflict >> layout [
	^ layout
]

{ #category : #accessing }
IncompatibleLayoutConflict >> layout: anObject [
	layout := anObject
]

{ #category : #accessing }
IncompatibleLayoutConflict >> messageText [
	^ 'Class with ', layout class name, ' cannot be extended as ', subType
]

{ #category : #accessing }
IncompatibleLayoutConflict >> subType [
	^ subType
]

{ #category : #accessing }
IncompatibleLayoutConflict >> subType: anObject [
	subType := anObject
]
