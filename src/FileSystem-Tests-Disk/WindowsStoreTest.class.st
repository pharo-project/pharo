"
SUnit tests for class WindowsStore
"
Class {
	#name : #WindowsStoreTest,
	#superclass : #TestCase,
	#category : #'FileSystem-Tests-Disk-Base'
}

{ #category : #testing }
WindowsStoreTest >> testAbsoluteFullName [
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.

	"Check dropping trailing slash."	
	self assert: (filesystem referenceTo: 'C:\tmp\') fullName equals: 'C:\tmp'.

	"Check round-trip conversion from String to FileReference back to String again."
	#( 	'C:\'		'D:\'		'\'		'C:\tmp'		'D:\tmp'		'C:\tmp\xx'      ) 
		do: [ :pathString | self assert: (filesystem referenceTo: pathString) fullName equals: pathString ].
	
	

]

{ #category : #testing }
WindowsStoreTest >> testAbsolutePath [

	#('c:\' 'C:\temp' 'A:\temp\test' '\test\bar' '\') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isAbsolute ] 
	
]

{ #category : #testing }
WindowsStoreTest >> testPrintString [
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
	
	"path"
	self assert: (filesystem referenceTo: 'a/b') printString equals: 'File @ a\b'.
	self assert: (filesystem referenceTo: '\test.txt') printString equals: 'File @ \test.txt'.
	
	"root"
	self assert: (filesystem referenceTo: '/') printString equals: 'File @ \'.
	self assert: (filesystem referenceTo: '\') printString equals: 'File @ \'.
	
	"drive root"
	self assert: (filesystem referenceTo: 'c:\') printString equals: 'File @ c:\'.
	
	"empty"
	self assert: (filesystem referenceTo: './') printString equals: 'File @ .'.
	self assert: (filesystem referenceTo: '.\') printString equals: 'File @ .'
]

{ #category : #testing }
WindowsStoreTest >> testRelativeFullName [

	"FIleSystem currently does not support MS Windows concept of each drive having its own working directory.
	That is too great a change during Pharo 3 Beta, so for now just drop drive from relative references. 
	Follow up in Case 13094"	

	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
		
	self assert: (filesystem referenceTo: 'C:tmp') fullName equals: (filesystem referenceTo: 'tmp') fullName
]

{ #category : #testing }
WindowsStoreTest >> testRelativePath [

	#('a' 'bin\foo' 'temp\test' 'C:temp\test') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isRelative ] 
	
]
