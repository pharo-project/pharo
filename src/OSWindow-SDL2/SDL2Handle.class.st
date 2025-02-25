"
My subclasses represent various handles coming from SDL2 library
"
Class {
	#name : 'SDL2Handle',
	#superclass : 'FFIExternalObject',
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'accessing' }
SDL2Handle class >> ffiLibrary [
	
	^ SDL2Library 

]

{ #category : 'nb-library' }
SDL2Handle >> ffiLibrary [
	
	^ SDL2Library 

]
