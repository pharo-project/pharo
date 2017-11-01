"
I'm a cairo surface who renders directly to a SDL2 surface.
"
Class {
	#name : #AthensCairoSDLSurface,
	#superclass : #AthensCairoSurface,
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'instance creation' }
AthensCairoSDLSurface class >> fromSDLSurface: surface [ 
	^ ( self 
		primImageFromData: surface pixels 
		width: surface w 
		height: surface h
		pitch: surface pitch) initialize
]

{ #category : #'instance creation' }
AthensCairoSDLSurface class >> newFromPixels: pixels width: width height: height pitch: pitch [
	^ (self
		primImageFromData: pixels
		width: width
		height: height
		pitch: pitch) initialize
]
