"
I'm a Set like collection that is used to hold collections of associated objects. When an object is added to or removed from this collection the inverse side is also updated.
"
Class {
	#name : #RelationSet,
	#superclass : #Collection,
	#instVars : [
		'owner',
		'slot',
		'set'
	],
	#category : #'Slot-Examples-Associations'
}

{ #category : #'instance creation' }
RelationSet class >> owner: anObject slot: aToManyAssociation [

	^self new initializeOwner: anObject slot: aToManyAssociation 
]

{ #category : #adding }
RelationSet >> add: anObject [

	(self includes: anObject)
		ifTrue: [ ^anObject ].
		
	slot add: anObject to: owner.
	^set add: anObject
]

{ #category : #copying }
RelationSet >> copy [

	^Set new 
		addAll: self;
		yourself
]

{ #category : #enumerating }
RelationSet >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	set do: aBlock
]

{ #category : #'initailize-release' }
RelationSet >> initializeOwner: anObject slot: aToManyAssociation [

	owner := anObject.
	slot := aToManyAssociation.
	set := Set new
]

{ #category : #internal }
RelationSet >> inverseAdd: anObject [

	^set add: anObject
]

{ #category : #internal }
RelationSet >> inverseRemove: anObject [

	^set remove: anObject
]

{ #category : #removing }
RelationSet >> remove: oldObject ifAbsent: aBlock [

	(set includes: oldObject)
		ifFalse: [ ^aBlock value ].
		
	slot remove: oldObject from: owner.
	^set remove: oldObject ifAbsent: aBlock
]

{ #category : #accessing }
RelationSet >> size [
	^set ifNil: [0] ifNotNil: [ set size ]
]

{ #category : #private }
RelationSet >> species [

	^Set
]
