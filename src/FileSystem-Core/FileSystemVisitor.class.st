"
I am an abstract superclass for objects that can perform operations on directory trees. My subclasses implement the visitor protocol, and process filesystem nodes shown to them by guides.
"
Class {
	#name : #FileSystemVisitor,
	#superclass : #Object,
	#category : #'FileSystem-Core-Base-Visitors'
}

{ #category : #visiting }
FileSystemVisitor >> visitDirectory: aReference [
	^ self visitReference: aReference
]

{ #category : #visiting }
FileSystemVisitor >> visitFile: aReference [
	^ self visitReference: aReference
]

{ #category : #visiting }
FileSystemVisitor >> visitReference: aReference [
]
