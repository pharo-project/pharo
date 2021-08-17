Class {
	#name : #SDL2ScanCodeConstants,
	#superclass : #SharedPool,
	#classVars : [
		'SDL_SCANCODE_COMMA',
		'SDL_SCANCODE_PERIOD'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #initialization }
SDL2ScanCodeConstants class >> initialize [

	self initializeScanCodes 
]

{ #category : #initialization }
SDL2ScanCodeConstants class >> initializeScanCodes [

	SDL_SCANCODE_COMMA := 54.
	SDL_SCANCODE_PERIOD := 55.
]
