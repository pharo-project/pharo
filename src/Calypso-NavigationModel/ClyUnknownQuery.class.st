"
I am special kind of query which implements NULL pattern.

I always return empty array as result and for any convertion methods I return myself.
Users should use my singleton #instance defined in class side: 	
	
	ClyUnknownQuery instance 

I am used as default query in Calypso-Browser widgets
"
Class {
	#name : #ClyUnknownQuery,
	#superclass : #ClyQuery,
	#classInstVars : [
		'instance'
	],
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #accessing }
ClyUnknownQuery class >> instance [
	^instance ifNil: [ instance := ClyUnknownQuery new ]
]

{ #category : #'merging queries' }
ClyUnknownQuery class >> mergeOwnInstances: queries [
	^queries anyOne
]

{ #category : #accessing }
ClyUnknownQuery class >> reset [
	instance := nil
]

{ #category : #converting }
ClyUnknownQuery >> asUnifiedInstance [
	^self
]

{ #category : #execution }
ClyUnknownQuery >> checkEmptyResult [
	^true
]

{ #category : #'item group decoration' }
ClyUnknownQuery >> decorateItemGroup: groupItem [
]

{ #category : #initialization }
ClyUnknownQuery >> defaultResult [
	^ClyUnknownQueryResult instance
]

{ #category : #testing }
ClyUnknownQuery >> executesQuery: aClass [ 
	^false
]

{ #category : #testing }
ClyUnknownQuery >> retrievesItemsOfType: itemTypeClass [ 
	^false
]
