"
Couple examples..
"
Class {
	#name : 'SDL2Example',
	#superclass : 'Object',
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : 'OSWindow-SDL2-Examples-Examples',
	#package : 'OSWindow-SDL2-Examples',
	#tag : 'Examples'
}

{ #category : 'examples' }
SDL2Example >> osWindow [
	"
	self new osWindow
	"

	| attrs window extent form |
	extent := 800 @ 600.
	form := Form extent: extent depth: 32.
	attrs := OSWindowAttributes new.
	attrs
		preferableDriver: OSSDL2Driver new;
		extent: extent.
	window := OSWindow createWithAttributes: attrs.
	window eventHandler:
		(OSWindowMorphicEventHandler for: self currentWorld).
	window newFormRenderer: form.
	^ window
]

{ #category : 'examples' }
SDL2Example >> simpleBlackWindow [
	"
	self new simpleBlackWindow
	"
	| window renderer |
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 640
					height: 480
					flags: SDL_WINDOW_SHOWN.
	renderer := window createDefaultRenderer.
	renderer drawColorR: 0 g: 0 b: 0 a: 0;
			  clear;
			  present.
	SDL2 delay: 3000.
	renderer destroy.
	window destroy
]

{ #category : 'examples' }
SDL2Example >> simpleDrawWindow [
	"
	self new simpleDrawWindow
	"
	| window renderer |
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 640
					height: 480
					flags: SDL_WINDOW_SHOWN.
	renderer := window createDefaultRenderer.
	SDL2 delay: 1000.
	renderer drawColorR: 0 g: 0 b: 0 a: 0;
			  clear;
			  drawColorR: 0 g: 255 b: 0 a: 255;
			  drawPointX: 50 y: 50;
			  present.
	SDL2 delay: 1000.
	renderer
			  drawColorR: 255 g: 0 b: 0 a: 255;
			  drawLineX1: 30 y1: 30 x2: 500 y2: 30;
			  present.
	SDL2 delay: 3000.
	renderer destroy.
	window destroy
]

{ #category : 'examples' }
SDL2Example >> simpleWindow [
	"
	self new simpleWindow
	"
	| window |
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 640
					height: 480
					flags: SDL_WINDOW_SHOWN.
	SDL2 delay: 3000.
	window destroy
]
