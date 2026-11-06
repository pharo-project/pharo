"
I am an abstract file handle.
"
Class {
	#name : 'AbstractFile',
	#superclass : 'FFIExternalObject',
	#instVars : [
		'reference'
	],
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'public' }
AbstractFile class >> open: aReference writable: aBoolean [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFile >> binaryReadStream [
	^ NewBinaryFileStream forFile: self forWrite: false
]

{ #category : 'accessing' }
AbstractFile >> binaryStream [
	^ NewBinaryFileStream forFile: self forWrite: self isForWrite
]

{ #category : 'accessing' }
AbstractFile >> binaryWriteStream [
	self assert: self isForWrite.
	^ NewBinaryFileStream forFile: self forWrite: self isForWrite
]

{ #category : 'accessing' }
AbstractFile >> fileSystem [
	^ reference fileSystem
]

{ #category : 'testing' }
AbstractFile >> isForWrite [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFile >> reference [
	^ reference
]

{ #category : 'accessing' }
AbstractFile >> setReference: newReference [
	reference := newReference
]
