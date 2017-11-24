"
My subclasses represent various handles coming from SDL2 library
"
Class {
	#name : #SDL2Handle,
	#superclass : #FFIExternalObject,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #accessing }
SDL2Handle class >> ffiLibraryName [
	^ SDL2 ffiLibraryName
]

{ #category : #'nb-library' }
SDL2Handle >> ffiLibraryName [
	^self class ffiLibraryName
]
