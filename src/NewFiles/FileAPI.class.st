"
I am a strategy for for choosing between the old and new file implementation.
"
Class {
	#name : 'FileAPI',
	#superclass : 'Object',
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'private' }
FileAPI class >> currentFileClass [
	^ self useNewFile ifTrue: [ NewFile ] ifFalse: [ OldFile ]
]

{ #category : 'file api' }
FileAPI class >> deleteFile: path [
	^ self currentFileClass deleteFile: path
]

{ #category : 'file api' }
FileAPI class >> exists: path [
	^ self currentFileClass exists: path
]

{ #category : 'file api' }
FileAPI class >> openAlways: path [
	^ self currentFileClass openAlways: path
]

{ #category : 'file api' }
FileAPI class >> openCreateAlways: path [
	^ self currentFileClass openCreateAlways: path
]

{ #category : 'file api' }
FileAPI class >> openReadOnly: path [
	^ self currentFileClass openReadOnly: path
]

{ #category : 'private' }
FileAPI class >> useNewFile [
	^ NewFile primitiveIsAvailable
]
