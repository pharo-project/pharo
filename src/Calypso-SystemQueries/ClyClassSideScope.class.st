"
I represent class side of classes
"
Class {
	#name : #ClyClassSideScope,
	#superclass : #ClyMetaLevelClassScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyClassSideScope class >> defaultName [
	^'class side'
]

{ #category : #'class selection' }
ClyClassSideScope class >> metaLevelOf: aClass [
	^aClass classSide
]
