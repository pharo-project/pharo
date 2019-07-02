Class {
	#name : #MockTreeTableItem,
	#superclass : #Object,
	#instVars : [
		'title'
	],
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #accessing }
MockTreeTableItem class >> title: aName [
	^ self new title: aName
]

{ #category : #comparing }
MockTreeTableItem >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ title = anObject title
]

{ #category : #comparing }
MockTreeTableItem >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ title hash
]

{ #category : #accessing }
MockTreeTableItem >> name [
	^ self title.
]

{ #category : #accessing }
MockTreeTableItem >> title [
	^ title
]

{ #category : #accessing }
MockTreeTableItem >> title: anObject [
	title := anObject
]
