Extension { #name : #Color }

{ #category : #'*Deprecated70' }
Color >> isTranslucentColor [
	"This means: self isTranslucent, but isTransparent not"
	self deprecated: 'To be removed without replacement'.
	^ false
]
