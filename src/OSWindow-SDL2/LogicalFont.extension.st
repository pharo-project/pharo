Extension { #name : #LogicalFont }

{ #category : #'*OSWindow-SDL2' }
LogicalFont >> createCachedFontForSDL2GenericRenderer: renderer [
	^ renderer getOrCreateCachedFontFor: self realFont
]
