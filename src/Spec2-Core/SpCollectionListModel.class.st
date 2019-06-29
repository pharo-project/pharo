Class {
	#name : #SpCollectionListModel,
	#superclass : #Object,
	#instVars : [
		'announcer',
		'collection',
		'sortingBlockHolder'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpCollectionListModel class >> on: aCollection [
	
	^ self new
		collection: aCollection;
		yourself
]

{ #category : #adding }
SpCollectionListModel >> add: anItem [
	
	collection := collection copyWith: anItem.
	
	self refreshList.
	self announcer announce: (ValueChanged newValue: self)
	
]

{ #category : #accessing }
SpCollectionListModel >> announcer [ 

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
SpCollectionListModel >> at: anIndex [

	^ collection at: anIndex
]

{ #category : #accessing }
SpCollectionListModel >> at: anIndex ifAbsent: aBlock [

	^ collection at: anIndex ifAbsent: aBlock
]

{ #category : #accessing }
SpCollectionListModel >> collection [
	^ collection
]

{ #category : #accessing }
SpCollectionListModel >> collection: anObject [
	| oldValue |

	oldValue := collection.
	collection := anObject.
	self refreshList.
	
	self announcer announce: (ValueChanged 
		oldValue: oldValue 
		newValue: collection)
]

{ #category : #accessing }
SpCollectionListModel >> indexOf: anIndex ifAbsent: aBlock [

	^ collection indexOf: anIndex ifAbsent: aBlock
]

{ #category : #initialization }
SpCollectionListModel >> initialize [
	super initialize.
	sortingBlockHolder := nil asValueHolder
]

{ #category : #accessing }
SpCollectionListModel >> items [

	^ collection
]

{ #category : #refreshing }
SpCollectionListModel >> refreshList [
	self sortingBlock ifNotNil: [ :aSortFunction | collection sort: aSortFunction ]
]

{ #category : #collection }
SpCollectionListModel >> removeAll [
	
	collection := #().
	
	self refreshList.
	self announcer announce: (ValueChanged newValue: self)
		
]

{ #category : #accessing }
SpCollectionListModel >> size [

	^ collection size
]

{ #category : #initialization }
SpCollectionListModel >> sortingBlock [
	^ sortingBlockHolder value
]

{ #category : #initialization }
SpCollectionListModel >> sortingBlock: aBlock [
	sortingBlockHolder value: aBlock.
	self refreshList
]

{ #category : #events }
SpCollectionListModel >> whenChangedDo: aBlock [
	
	self announcer 
		when: ValueChanged 
		do: [ :ann |
			aBlock 
				cull: ann newValue 
				cull: ann oldValue
				cull: ann ]
]

{ #category : #events }
SpCollectionListModel >> whenSortingBlockChangedDo: aBlock [
	sortingBlockHolder whenChangedDo: aBlock
]
