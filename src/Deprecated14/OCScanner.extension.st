Extension { #name : 'OCScanner' }

{ #category : '*Deprecated14' }
OCScanner >> errorBlock: aBlock [
	self deprecated: 'No-op as errorBlock is removed'
]

{ #category : '*Deprecated14' }
OCScanner class >> on: aStream errorBlock: aBlock [

	self
		deprecated: 'The scanner no more signals errors or execute error blocks, it just silently produces error tokens'
		transformWith: '`@receiver on: `@arg1 errorBlock: `@arg2' -> '`@receiver on: `@arg1'.
	^ self on: aStream
]

{ #category : '*Deprecated14' }
OCScanner class >> scanTokenObjects: aStringOrStream [
	"Return the tokens objects."

	self deprecated: 'Use scanTokens:' 
		on: '18/12/2024'  
		in: 'pharo13'
		transformWith:  '`@receiver scanTokenObjects: `@arg '
			-> '`@receiver scanTokens: `@arg '.
	^ self scanTokens: aStringOrStream
]
