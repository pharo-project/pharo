"
I hold metadata about a specific file memory mapped segment.
"
Class {
	#name : 'NewFileMemoryMappingView',
	#superclass : 'Object',
	#instVars : [
		'file',
		'size',
		'address',
		'protection'
	],
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'mocks' }
NewFileMemoryMappingView class >> readOnlyWithByteArray: byteArray [
	^ self new
		address: byteArray;
		size: byteArray size;
		protection: NewFileMemMapProtectionReadOnly;
		yourself
]

{ #category : 'mocks' }
NewFileMemoryMappingView class >> readWriteWithByteArray: byteArray [
	^ self new
		address: byteArray;
		size: byteArray size;
		protection: NewFileMemMapProtectionReadWrite;
		yourself
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> address [

	^ address
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> address: anObject [

	address := anObject
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> contents [
	^ address copyFrom: 1 to: size
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> file [

	^ file
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> file: anObject [

	file := anObject
]

{ #category : 'testing' }
NewFileMemoryMappingView >> hasWritePermission [
	^ protection = NewFileMemMapProtectionReadWrite
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> protection [

	^ protection
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> protection: anObject [

	protection := anObject
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> size [

	^ size
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> size: anObject [

	size := anObject
]

{ #category : 'accessing' }
NewFileMemoryMappingView >> stream [
	^ NewFileMemoryMappedStream onFileMemoryMappingView: self
]

{ #category : 'as yet unclassified' }
NewFileMemoryMappingView >> unmap [
	file ifNil: [ ^ self ].
	^ file unmapView: self
]

{ #category : 'as yet unclassified' }
NewFileMemoryMappingView >> unmapped [
	address := nil.
	file := nil.
	size := 0.
]
