"
SUnit tests for a single file system tree (depth one)
"
Class {
	#name : #SingleTreeTest,
	#superclass : #FileSystemTreeTest,
	#instVars : [
		'filesystem'
	],
	#category : #'FileSystem-Tests-Core-Base'
}

{ #category : #testing }
SingleTreeTest class >> isAbstract [
	^ self name = #SingleTreeTest
]

{ #category : #running }
SingleTreeTest >> createDirectory: aString [ 
	filesystem createDirectory: (filesystem pathFromString: aString)
]

{ #category : #running }
SingleTreeTest >> createFile: aString [ 
	filesystem store createFile: (filesystem pathFromString: aString)
]

{ #category : #running }
SingleTreeTest >> setUp [
	super setUp.
	filesystem := FileSystem memory.
	
]
