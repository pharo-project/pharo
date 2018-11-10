"
I'm a helper to keep selected items in a list.
"
Class {
	#name : #IceGitHubSelectItem,
	#superclass : #Object,
	#instVars : [
		'list',
		'item'
	],
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #'instance creation' }
IceGitHubSelectItem class >> list: selectedItems item: anObject [
	^ self basicNew 
		initializeList: selectedItems item: anObject;
		yourself
]

{ #category : #'instance creation' }
IceGitHubSelectItem class >> new [
	self error: 'Use #list:item:'
]

{ #category : #private }
IceGitHubSelectItem >> addItem [
	(list includes: item) ifTrue: [ ^ self ].
	list add: item 
]

{ #category : #initialization }
IceGitHubSelectItem >> initializeList: aCollection item: anObject [
	list := aCollection.
	item := anObject.
	self initialize
]

{ #category : #private }
IceGitHubSelectItem >> removeItem [
	(list includes: item) ifFalse: [ ^ self ].
	list remove: item 
]

{ #category : #accessing }
IceGitHubSelectItem >> value [ 
	^ list includes: item
]

{ #category : #accessing }
IceGitHubSelectItem >> value: aBoolean [
	aBoolean 
		ifTrue: [ self addItem ]
		ifFalse: [ self removeItem ]
	
]
