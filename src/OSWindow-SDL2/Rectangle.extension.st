Extension { #name : #Rectangle }

{ #category : #'*OSWindow-SDL2' }
Rectangle >> asSDLRect [
	
	^SDL_Rect newX: origin x rounded y: origin y rounded w: (corner x - origin x) rounded h: (corner y - origin y) rounded.
]
