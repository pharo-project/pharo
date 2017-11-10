Class {
	#name : #GLMAnnouncingCollection,
	#superclass : #Announcer,
	#instVars : [
		'collection'
	],
	#category : #Glamour-Tests-Resources
}

{ #category : #initialization }
GLMAnnouncingCollection >> add: anObject [

	collection add: anObject.
	self announce: GLMItemAdded
]

{ #category : #initialization }
GLMAnnouncingCollection >> add: newObject before: oldObject [ 

	collection add: newObject before: oldObject.
	self announce: GLMItemAdded
]

{ #category : #initialization }
GLMAnnouncingCollection >> add: newObject beforeOrLast: oldObject [ 
	(collection includes: oldObject)
		ifTrue: [ collection add: newObject before: oldObject ]
		ifFalse: [ collection add: newObject ].
	self announce: GLMItemAdded
]

{ #category : #converting }
GLMAnnouncingCollection >> asOrderedCollection [
	^ self
]

{ #category : #initialization }
GLMAnnouncingCollection >> at: anObject [

	^collection at: anObject
]

{ #category : #private }
GLMAnnouncingCollection >> collect: aBlock [
	^collection collect: aBlock
	
]

{ #category : #initialization }
GLMAnnouncingCollection >> do: aBlock [

	^collection do: aBlock
]

{ #category : #private }
GLMAnnouncingCollection >> includes: anObject [ 
	^collection includes: anObject
]

{ #category : #initialization }
GLMAnnouncingCollection >> indexOf: anElement [

	^ collection indexOf: anElement
]

{ #category : #initialization }
GLMAnnouncingCollection >> initialize [
	super initialize.
	collection := OrderedCollection new
]

{ #category : #initialization }
GLMAnnouncingCollection >> isCollection [

	^true
]

{ #category : #testing }
GLMAnnouncingCollection >> isEmpty [
	^ collection isEmpty
]

{ #category : #initialization }
GLMAnnouncingCollection >> printString [
	^ collection printString 
]

{ #category : #initialization }
GLMAnnouncingCollection >> remove: anObject [

	collection remove: anObject.
	self announce: GLMItemRemoved
]

{ #category : #initialization }
GLMAnnouncingCollection >> remove: anObject ifAbsent: aBlock [

	collection remove: anObject ifAbsent: aBlock.
	self announce: GLMItemRemoved
]

{ #category : #initialization }
GLMAnnouncingCollection >> removeLast [

	collection removeLast.
	self announce: GLMItemRemoved
]

{ #category : #initialization }
GLMAnnouncingCollection >> size [
	^ collection size
]

{ #category : #initialization }
GLMAnnouncingCollection >> withIndexDo: aBlock [

	^collection withIndexDo: aBlock
]
