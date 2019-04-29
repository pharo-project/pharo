Class {
	#name : #SpecCollectionListModel,
	#superclass : #Object,
	#instVars : [
		'announcer',
		'collection',
		'filter',
		'shownCollection',
		'changedBlock',
		'sortingBlockHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpecCollectionListModel class >> on: aCollection [
	
	^ self new
		collection: aCollection;
		yourself
]

{ #category : #adding }
SpecCollectionListModel >> add: anItem [
	
	collection := collection copyWith: anItem.
	
	self refreshList.
	self announcer announce: (ValueChanged newValue: self)
	
]

{ #category : #accessing }
SpecCollectionListModel >> announcer [ 

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
SpecCollectionListModel >> at: anIndex [

	^ collection at: anIndex
]

{ #category : #accessing }
SpecCollectionListModel >> at: anIndex ifAbsent: aBlock [

	^ collection at: anIndex ifAbsent: aBlock
]

{ #category : #accessing }
SpecCollectionListModel >> collection [
	^ collection
]

{ #category : #accessing }
SpecCollectionListModel >> collection: anObject [
	| oldValue |

	oldValue := collection.
	collection := anObject.
	self refreshList.
	
	self announcer announce: (ValueChanged 
		oldValue: oldValue 
		newValue: collection)
]

{ #category : #accessing }
SpecCollectionListModel >> filterWith: aBlockClosure [ 
	
	filter := aBlockClosure.
	self refreshList.
]

{ #category : #accessing }
SpecCollectionListModel >> indexOf: anIndex ifAbsent: aBlock [

	^ collection indexOf: anIndex ifAbsent: aBlock
]

{ #category : #initialization }
SpecCollectionListModel >> initialize [

	super initialize.
	filter := [ true ].
	sortingBlockHolder := nil asValueHolder
]

{ #category : #accessing }
SpecCollectionListModel >> items [

	^ collection
]

{ #category : #refreshing }
SpecCollectionListModel >> refreshList [
	self sortingBlock ifNotNil: [ :aSortFunction | collection sort: aSortFunction ].
	shownCollection := collection select: [ :elem | filter cull: elem cull: collection ].
]

{ #category : #collection }
SpecCollectionListModel >> removeAll [
	
	collection := #().
	
	self refreshList.
	self announcer announce: (ValueChanged newValue: self)
		
]

{ #category : #accessing }
SpecCollectionListModel >> shownItems [
	
	^ shownCollection
]

{ #category : #accessing }
SpecCollectionListModel >> size [

	^ collection size
]

{ #category : #initialization }
SpecCollectionListModel >> sortingBlock [
	^ sortingBlockHolder value
]

{ #category : #initialization }
SpecCollectionListModel >> sortingBlock: aBlock [
	sortingBlockHolder value: aBlock.
	self refreshList
]

{ #category : #events }
SpecCollectionListModel >> whenChangedDo: aBlock [
	
	self announcer 
		when: ValueChanged 
		do: [ :ann |
			aBlock 
				cull: ann newValue 
				cull: ann oldValue
				cull: ann ]
]

{ #category : #events }
SpecCollectionListModel >> whenSortingBlockChangedDo: aBlock [
	sortingBlockHolder whenChangedDo: aBlock
]
