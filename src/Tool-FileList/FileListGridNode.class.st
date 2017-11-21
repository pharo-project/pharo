"
A node in the file list grid
"
Class {
	#name : #FileListGridNode,
	#superclass : #MorphTreeNodeModel,
	#category : #Tool-FileList
}

{ #category : #'user interface' }
FileListGridNode >> fileName [
	^ self theme newTextIn: self text: item basename
]

{ #category : #'user interface' }
FileListGridNode >> filePermissions [
	^ self theme newTextIn: self text: item permissions asString
]

{ #category : #'user interface' }
FileListGridNode >> fileSize [
	^ self theme newTextIn: self text: item humanReadableSize asString
]

{ #category : #'user interface' }
FileListGridNode >> modificationDate [
	^ self theme newTextIn: self text: item modificationTime asString
]
