"
I'm an abstract superclass for enumeration operations on directory entries. 
"
Class {
	#name : #AbstractEnumerationVisitor,
	#superclass : #FileSystemVisitor,
	#instVars : [
		'out',
		'block'
	],
	#category : #'FileSystem-Core-Base-Visitors'
}

{ #category : #visiting }
AbstractEnumerationVisitor >> breadthFirst: aReference [
	^ self visit: aReference with: (BreadthFirstGuide for: self)
]

{ #category : #initialization }
AbstractEnumerationVisitor >> initializeWithBlock: aBlock [
	self initialize.
	block := aBlock
]

{ #category : #visiting }
AbstractEnumerationVisitor >> postorder: aReference [
	^ self visit: aReference with: (PostorderGuide for: self)
]

{ #category : #visiting }
AbstractEnumerationVisitor >> preorder: aReference [
	^ self visit: aReference with: (PreorderGuide for: self)
]

{ #category : #visiting }
AbstractEnumerationVisitor >> visit: aReference with: aGuide [
	out := (Array new: 10) writeStream.
	aGuide show: aReference.
	^ out contents
]

{ #category : #visiting }
AbstractEnumerationVisitor >> visitReference: anEntry [
	self subclassResponsibility
]
