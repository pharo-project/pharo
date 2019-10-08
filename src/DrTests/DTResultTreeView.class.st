"
I model a tree view that can be created on a DTPluginResult.
"
Class {
	#name : #DTResultTreeView,
	#superclass : #Object,
	#instVars : [
		'name',
		'blockToExtractViewFromResult'
	],
	#category : #'DrTests-Model'
}

{ #category : #'instance creation' }
DTResultTreeView class >> name: aString blockToExtractViewFromResult: blockToExtractViewFromResult [
	^ self new
		name: aString;
		blockToExtractViewFromResult: blockToExtractViewFromResult;
		yourself
]

{ #category : #accessing }
DTResultTreeView >> blockToExtractViewFromResult [
	^ blockToExtractViewFromResult
]

{ #category : #accessing }
DTResultTreeView >> blockToExtractViewFromResult: anObject [
	blockToExtractViewFromResult := anObject
]

{ #category : #accessing }
DTResultTreeView >> name [
	^ name
]

{ #category : #accessing }
DTResultTreeView >> name: anObject [
	name := anObject
]

{ #category : #accessing }
DTResultTreeView >> resultTreeFor: result [
	result ifNil: [ 
		^ DTTreeNode empty ].
	^ self blockToExtractViewFromResult value: result
]
