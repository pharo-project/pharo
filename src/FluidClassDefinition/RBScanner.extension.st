Extension { #name : #RBScanner }

{ #category : #'*FluidClassDefinition' }
RBScanner class >> scanTokenObjectsStef: aStringOrStream [
	"Return the tokens objects."
	
	| scanner |
	scanner := self on: aStringOrStream readStream.
	^ scanner contents
]
