"
A 32-bit ARGB Cursor of arbitrary extent (some platforms may limit the size). Compositing assumes alpha is pre-multiplied.
"
Class {
	#name : 'CursorWithAlpha',
	#superclass : 'Cursor',
	#instVars : [
		'fallback'
	],
	#category : 'Graphics-Display Objects-Cursors',
	#package : 'Graphics-Display Objects',
	#tag : 'Cursors'
}

{ #category : 'converting' }
CursorWithAlpha >> asCursorForm [

	^ Form newFrom: self
]

{ #category : 'accessing' }
CursorWithAlpha >> fallback [
	^fallback ifNil: [NormalCursor]
]

{ #category : 'accessing' }
CursorWithAlpha >> fallback: aCursor [
	fallback := aCursor
]
