"
I represent how many local children exist for object in concrete query result.

For example if you look at classes as a hierarchy then you can see Object and its subclasses. This hierarchy can be limited by package scope.
So in case of one particular package Object can have 10 subclasses. But in another package it can be 30 (for example).
And I represents such local hierarchy size. To access it from browser item use following methods:
	- aBrowserItem localHierarchySize
	- aBrowserItem localHierarchySize: count
I am used by tools to organize tree view for list of items which provide local hierarchy by themselves. Item has no real list of children. But instead it knows count of internal tree. It allows tool hide right number of items when given parent node needs to be collapsed. Important condition here is that I must hold count of full subtree of local hierarchy (not just first level children).
 
To manually create my instances use:
	ClyItemLocalHierarchyProperty size: 10

Internal Representation and Key Implementation Points.

    Instance Variables
	subtreeSize:		<Integer>
"
Class {
	#name : #ClyItemLocalHierarchyProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'subtreeSize'
	],
	#category : #'Calypso-NavigationModel-Property'
}

{ #category : #'instance creation' }
ClyItemLocalHierarchyProperty class >> size: subtreeSize [
	^self new 
		subtreeSize: subtreeSize
]

{ #category : #printing }
ClyItemLocalHierarchyProperty >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: 'size: '.
	subtreeSize printOn: aStream.
	aStream nextPut: $).
]

{ #category : #accessing }
ClyItemLocalHierarchyProperty >> subtreeSize [
	^ subtreeSize
]

{ #category : #accessing }
ClyItemLocalHierarchyProperty >> subtreeSize: anObject [
	subtreeSize := anObject
]
