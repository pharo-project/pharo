Extension { #name : #RBScanner }

{ #category : #'*Deprecated11' }
RBScanner class >> on: aStream errorBlock: aBlock [

	self
		deprecated: 'The scanner no more signals errors or execute error blocks, it just silently produces error tokens'
		transformWith: '`@receiver on: `@arg` errorBlock: `@arg2' -> '`@receiver on: `@arg1'.
	^ self on: aStream
]

{ #category : #'*Deprecated11' }
RBScanner class >> scanTokens: aStringOrStream [
	"Return the tokens (and not the scanner token objects)."
	self deprecated: 'Unused in standard image Pharo 11, therefore deprecated without replacement'.
	
	^ (self scanTokenObjects: aStringOrStream) collect: [ :t | t value ]
]
