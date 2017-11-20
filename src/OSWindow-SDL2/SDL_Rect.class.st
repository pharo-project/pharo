"
SDL2 library structure
"
Class {
	#name : #SDL_Rect,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_H',
		'OFFSET_W',
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'fields description' }
SDL_Rect class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^#(
		int x;
		int y;
		int w;
		int h;
 	)
]

{ #category : #'fields description' }
SDL_Rect class >> newX: x y: y w: w h: h [
	^ self new x: x; y: y; w: w; h: h; yourself
]

{ #category : #'accessing structure variables' }
SDL_Rect >> h [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_H
]

{ #category : #'accessing structure variables' }
SDL_Rect >> h: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_H put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Rect >> w [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_W
]

{ #category : #'accessing structure variables' }
SDL_Rect >> w: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_W put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Rect >> x [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_Rect >> x: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Rect >> y [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_Rect >> y: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Y put: anObject
]
