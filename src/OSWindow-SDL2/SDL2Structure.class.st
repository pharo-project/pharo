"
Common things for all SDL2 structs
"
Class {
	#name : 'SDL2Structure',
	#superclass : 'FFIExternalStructure',
	#pools : [
		'SDL2',
		'SDL2Constants',
		'SDL2ScanCodeConstants',
		'SDL2Types'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'native' }
SDL2Structure class >> ffiLibrary [
	
	^ SDL2Library 

]

{ #category : 'native' }
SDL2Structure >> ffiLibrary [
	
	^ SDL2Library 

]
