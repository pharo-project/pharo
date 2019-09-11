"
I represent instance side of classes
"
Class {
	#name : #ClyInstanceSideScope,
	#superclass : #ClyMetaLevelClassScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyInstanceSideScope class >> defaultName [
	^'inst. side'
]

{ #category : #'class selection' }
ClyInstanceSideScope class >> metaLevelOf: aClass [
	^aClass instanceSide
]
