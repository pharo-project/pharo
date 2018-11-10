"
I represent a directory inside the iceberg repository. 

My contents is an empty string.

See my superclasses for more information.
"
Class {
	#name : #IceDirectoryDefinition,
	#superclass : #IceFileSystemDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceDirectoryDefinition >> accept: aVisitor [
	
	^ aVisitor visitDirectoryDefinition: self
]

{ #category : #accessing }
IceDirectoryDefinition >> contents [
	
	^ ''
]

{ #category : #testing }
IceDirectoryDefinition >> isDirectoryDefinition [
	
	^ true
]
