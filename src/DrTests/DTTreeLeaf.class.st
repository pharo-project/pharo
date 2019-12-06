"
I am a leaf of a Result Tree.
I have te content and I know the way to browse myself. 
"
Class {
	#name : #DTTreeLeaf,
	#superclass : #DTAbstractTreeNode,
	#instVars : [
		'content'
	],
	#category : #'DrTests-Model'
}

{ #category : #'instance creation' }
DTTreeLeaf class >> content: aRottenTest [ 
	^ self new
		content: aRottenTest;
		yourself
]

{ #category : #visiting }
DTTreeLeaf >> acceptVisitor: aDTResultsTreeVisitor [
	^ aDTResultsTreeVisitor visitDTTreeLeaf: self
]

{ #category : #testing }
DTTreeLeaf >> canBeBrowsed [
	^ true
]

{ #category : #accessing }
DTTreeLeaf >> content [
	^ content
]

{ #category : #accessing }
DTTreeLeaf >> content: anObject [
	content := anObject
]

{ #category : #accessing }
DTTreeLeaf >> contentForReRun [
	^ { self content }
]

{ #category : #actions }
DTTreeLeaf >> drTestsBrowse [
	self content drTestsBrowse
]

{ #category : #menu }
DTTreeLeaf >> drTestsBuildContextMenu: menu [
	self content drTestsBuildContextMenu: menu
]

{ #category : #accessing }
DTTreeLeaf >> drTestsName [
	^ self content drTestsName
]

{ #category : #testing }
DTTreeLeaf >> isLeaf [
	^ true
]
