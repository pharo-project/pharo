"
Common things for all SDL2 structs
"
Class {
	#name : #SDL2Structure,
	#superclass : #FFIExternalStructure,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #native }
SDL2Structure class >> ffiLibraryName [
	^ SDL2 ffiLibraryName
]

{ #category : #native }
SDL2Structure >> ffiLibraryName [
	^self class ffiLibraryName
]
