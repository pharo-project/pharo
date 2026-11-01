"
I am streaming interface for memory mapped files.
"
Class {
	#name : 'NewFileMemoryMappedStream',
	#superclass : 'Stream',
	#instVars : [
		'mappingView',
		'address',
		'position',
		'size',
		'hasWritePermission'
	],
	#category : 'NewFiles-Streams',
	#package : 'NewFiles',
	#tag : 'Streams'
}

{ #category : 'as yet unclassified' }
NewFileMemoryMappedStream class >> onFileMemoryMappingView: mappingView [
	^ self basicNew
		initializeWithFileMemoryMappingView: mappingView;
		yourself
]

{ #category : 'testing' }
NewFileMemoryMappedStream >> atEnd [
	^ position >= size
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> contents [
	^ mappingView contents
]

{ #category : 'initialization' }
NewFileMemoryMappedStream >> initializeWithFileMemoryMappingView: initialMappingView [
	mappingView := initialMappingView.
	address := initialMappingView address.
	position := 0.
	size := initialMappingView size.
	hasWritePermission := initialMappingView hasWritePermission.
	
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> next [
	| result |
	position >= size ifTrue: [ ^ nil ].
	result := address uint8AtOffset: position.
	position := position + 1.
	^ result
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> next: n [
	| remainingSize resultSize buffer |
	remainingSize := size - position.
	resultSize := n min: remainingSize.
	buffer := ByteArray new: n.
	1 to: resultSize do: [:i |
		buffer at: i put: (address uint8AtOffset: position).
		position := position + 1
	].
	^ buffer
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> next: amount putAll: aByteArray [

	^ self next: amount putAll: aByteArray startingAt: 1
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> next: amount putAll: aByteArray startingAt: startingIndex [
	| remainingSize putAmount |
	hasWritePermission ifFalse: [
		^ self error: 'Cannot write into a read-only file memory mapping'
	].
	remainingSize := size - position.
	putAmount := amount min: remainingSize.
	putAmount > 0 ifTrue: [
		address replaceFrom: position + 1 to: position + putAmount with: aByteArray startingAt: startingIndex
	].
	^ aByteArray 
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> nextPut: value [
	| result |
	hasWritePermission ifFalse: [
		^ self error: 'Cannot write into a read-only file memory mapping'
	].

	position >= size ifTrue: [ ^ nil ].
	result := address uint8AtOffset: position put: value.
	position := position + 1.
	^ result
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> nextPutAll: aByteArray [
	aByteArray class = ByteArray ifFalse: [
		^ super nextPutAll: aByteArray
	].

	^ self next: aByteArray basicSize putAll: aByteArray
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> position [

	^ position
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> position: anObject [

	position := anObject
]

{ #category : 'accessing' }
NewFileMemoryMappedStream >> size [

	^ size
]
