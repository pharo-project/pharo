"
I represent the scroll direction types for the Scroll event.

I'm a binding to the following enum in SDL2 library:

```
typedef enum
{
    SDL_MOUSEWHEEL_NORMAL,    /**< The scroll direction is normal */
    SDL_MOUSEWHEEL_FLIPPED    /**< The scroll direction is flipped / natural */
} SDL_MouseWheelDirection;
```


"
Class {
	#name : 'SDL_MouseWheelDirection',
	#superclass : 'FFIEnumeration',
	#classVars : [
		'SDL_MOUSEWHEEL_FLIPPED',
		'SDL_MOUSEWHEEL_NORMAL'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'accessing enum' }
SDL_MouseWheelDirection class >> SDL_MOUSEWHEEL_FLIPPED [
	"This method was automatically generated"
	^ SDL_MOUSEWHEEL_FLIPPED
]

{ #category : 'accessing enum' }
SDL_MouseWheelDirection class >> SDL_MOUSEWHEEL_NORMAL [
	"This method was automatically generated"
	^ SDL_MOUSEWHEEL_NORMAL
]

{ #category : 'enum declaration' }
SDL_MouseWheelDirection class >> enumDecl [
	"
	self rebuildEnumAccessors
	"

	^ #(
    SDL_MOUSEWHEEL_NORMAL  0    "The scroll direction is normal"
    SDL_MOUSEWHEEL_FLIPPED 1    "The scroll direction is flipped"
	)
]

{ #category : 'class initialization' }
SDL_MouseWheelDirection class >> initialize [
	"Duplicate to ensure being called at load time"
	self initializeEnumeration.
]
