"
My instances implement OSWindowGenericRenderer interface by wrapping a SDL2 renderer.
"
Class {
	#name : #OSSDL2GenericRenderer,
	#superclass : #OSWindowGenericRenderer,
	#instVars : [
		'renderer'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #OSWindow-SDL2
}

{ #category : #rendering }
OSSDL2GenericRenderer >> clear [
	renderer clear
]

{ #category : #rendering }
OSSDL2GenericRenderer >> color: aColor [
	| red green blue alpha |
	red := self mapColorChannel: aColor red.
	green := self mapColorChannel: aColor green.
	blue := self mapColorChannel: aColor blue.
	alpha := self mapColorChannel: aColor alpha.
	renderer drawColorR: red g: green b: blue a: alpha
]

{ #category : #initialization }
OSSDL2GenericRenderer >> createSDL2Renderer [
	renderer := window handle createDefaultRenderer.
]

{ #category : #deleting }
OSSDL2GenericRenderer >> destroy [
	renderer destroy
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawLine: startPoint end: endPoint [
	renderer drawLineX1: startPoint x asInteger y1: startPoint y asInteger x2: endPoint x asInteger y2: endPoint y asInteger
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawPoint: aPoint [
	renderer drawPointX: aPoint x asInteger y: aPoint y asInteger
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawRectangle: aRectangle [
	renderer drawRect: aRectangle asSDLRect
]

{ #category : #rendering }
OSSDL2GenericRenderer >> fillRectangle: aRectangle [
	renderer fillRect: aRectangle asSDLRect
]

{ #category : #private }
OSSDL2GenericRenderer >> mapColorChannel: colorChannel [
	^ colorChannel * 255 asInteger min: 255 max: 0
]

{ #category : #rendering }
OSSDL2GenericRenderer >> present [
	renderer present
]
