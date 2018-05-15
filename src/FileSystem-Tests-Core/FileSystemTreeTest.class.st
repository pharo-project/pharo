"
SUnit tests for a file system tree
"
Class {
	#name : #FileSystemTreeTest,
	#superclass : #TestCase,
	#category : #'FileSystem-Tests-Core-Base'
}

{ #category : #testing }
FileSystemTreeTest class >> isAbstract [
	^ self name = #FileSystemTreeTest
]

{ #category : #running }
FileSystemTreeTest >> createDirectory: aString [
	self subclassResponsibility
]

{ #category : #running }
FileSystemTreeTest >> createFile: aString [
	self subclassResponsibility
]

{ #category : #running }
FileSystemTreeTest >> setUpGreek [
	self 
		createDirectory: '/alpha';
		createDirectory: '/alpha/beta';
		createFile: '/alpha/beta/gamma';
		createFile: '/alpha/beta/delta';
		createDirectory: '/alpha/epsilon';
		createFile: '/alpha/epsilon/zeta'
]
