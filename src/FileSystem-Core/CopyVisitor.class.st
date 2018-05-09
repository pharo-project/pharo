"
I create a copy of the directory tree that I visit. I use the PreorderGuide so that I can create directories before creating their contents. 
"
Class {
	#name : #CopyVisitor,
	#superclass : #FileSystemVisitor,
	#instVars : [
		'source',
		'dest'
	],
	#category : #'FileSystem-Core-Base-Visitors'
}

{ #category : #'instance creation' }
CopyVisitor class >> copy: source to: dest [
	(self from: source to: dest) visit
]

{ #category : #'instance creation' }
CopyVisitor class >> from: srcReference to: dstReference [
	^ self basicNew
		initializeWithSource: srcReference 
		dest: dstReference
]

{ #category : #visiting }
CopyVisitor >> copyDirectory: aReference [
	| directory |
	directory := dest resolve: (aReference relativeTo: source).
	directory createDirectory
]

{ #category : #visiting }
CopyVisitor >> copyFile: aReference [
	| copy |
	copy := dest resolve: (aReference relativeTo: source).
	aReference copyTo: copy
]

{ #category : #'initialize-release' }
CopyVisitor >> initializeWithSource: srcReference dest: dstReference [
	self initialize.
	source := srcReference.
	dest := dstReference
]

{ #category : #visiting }
CopyVisitor >> visit [
	(PreorderGuide for: self) 
		show: source
]

{ #category : #visiting }
CopyVisitor >> visitDirectory: anEntry [
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [dest ensureCreateDirectory]
		ifFalse: [self copyDirectory: reference]
]

{ #category : #visiting }
CopyVisitor >> visitFile: anEntry [
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [source copyTo: dest]
		ifFalse: [self copyFile: reference]
]
