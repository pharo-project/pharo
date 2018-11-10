"
I'm a base for all kind of plugins. 
I can define 

- repositoryActions (actions that will be added to IceRepositoriesBrowser, repositories section)
- ... (TBD)
"
Class {
	#name : #IcePlugin,
	#superclass : #Object,
	#category : #'Iceberg-Plugin-Base'
}

{ #category : #'as yet unclassified' }
IcePlugin class >> allAvailableFor: aRepository [
	^ self allSubclasses select: [ :each | each isAvailableFor: aRepository ]

]

{ #category : #testing }
IcePlugin class >> isAvailableFor: aRepository [
	^ self subclassResponsibility
]

{ #category : #'accessing-ui' }
IcePlugin >> checkoutBranchPanelsOnModel: anIceTipCachedModel [ 

	"Hook for subclasses"
	^ #()
]

{ #category : #events }
IcePlugin >> repositoryWasCreated: aRepository [
	"Hook. Allows plugins to do an action when a repository is created"

]

{ #category : #events }
IcePlugin >> repositoryWillBeCreated: aRepository [
	"Hook. Allows plugins to do an action when a repository will be added"

]
