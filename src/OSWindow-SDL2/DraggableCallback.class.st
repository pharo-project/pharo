"
I represent a callback to make the window draggable from any chosen rectangle. Useful for undecorated windows.

Warning : Only available with SDL 2.0.4 or higher !
"
Class {
	#name : #DraggableCallback,
	#superclass : #FFICallback,
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'callback spec' }
DraggableCallback class >> fnSpec [

	^'int ( SDL_Window aWindow, SDL_Point* pt, void* data )'
]

{ #category : #'instance creation' }
DraggableCallback class >> on: aBlock [
	^ self 
		signature: self fnSpec
		block: aBlock
]
