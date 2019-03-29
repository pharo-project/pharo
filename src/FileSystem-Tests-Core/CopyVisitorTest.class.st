"
SUnit tests for CopyVisitor
"
Class {
	#name : #CopyVisitorTest,
	#superclass : #FileSystemTreeTest,
	#instVars : [
		'source',
		'dest'
	],
	#category : #'FileSystem-Tests-Core-Visitors'
}

{ #category : #running }
CopyVisitorTest >> createDirectory: aString [ 
	source createDirectory: (source store pathFromString: aString)
]

{ #category : #running }
CopyVisitorTest >> createFile: aString [ 
	source store createFile: (source store pathFromString: aString)
]

{ #category : #running }
CopyVisitorTest >> setUp [
	super setUp.
	source := FileSystem memory.
	dest := FileSystem memory
]

{ #category : #tests }
CopyVisitorTest >> testAll [
	self setUpGreek.
	CopyVisitor 
		copy: (source / 'alpha') 
		to: (dest / 'alpha').
	self assert: (dest isDirectory: '/alpha').
	self assert: (dest isFile: '/alpha/beta/gamma')
]
