Extension { #name : 'Rectangle' }

{ #category : '*OSWindow-SDL2' }
Rectangle >> asSDLRect [

	^ SDL_Rect
		  newX: self origin x rounded
		  y: self origin y rounded
		  w: (self corner x - self origin x) rounded
		  h: (self corner y - self origin y) rounded
]
