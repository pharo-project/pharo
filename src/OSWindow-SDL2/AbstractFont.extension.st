Extension { #name : #AbstractFont }

{ #category : #'*OSWindow-SDL2' }
AbstractFont >> createCachedFontForSDL2GenericRenderer: renderer [
	^ renderer createTextureAtlasFontCacheFor: self
]
