Extension { #name : 'CompilationContext' }

{ #category : '*Deprecated12' }
CompilationContext >> noPattern [
	self
		deprecated: 'Use #isScripting instead'
		transformWith: '`@receiver noPattern' -> '`@receiver isScripting'.
	^self isScripting
]

{ #category : '*Deprecated12' }
CompilationContext >> noPattern: aBoolean [

		self
		deprecated: 'Use #isScripting: instead'
		transformWith: '`@receiver noPattern: `@arg' -> '`@receiver isScripting: `@arg'.

	self isScripting: aBoolean
]
