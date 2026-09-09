Extension { #name : 'OpalCompiler' }

{ #category : '*Deprecated14' }
OpalCompiler >> compiledMethodTrailer: bytes [
	self deprecated: 'CompiledMethodTrailer has been removed in Pharo12. Just set the sourcePointer after compiling, see class comment of the deprecated class CompiledMethodTrailer'
]

{ #category : '*Deprecated14' }
OpalCompiler class >> evaluate: textOrString [
	self
		deprecated: 'Please use new compiler API instead'
		transformWith: '`@receiver evaluate: `@statements1 ' -> '`@receiver new source: `@statements1; evaluate'.
	^ self new
		source: textOrString;
		evaluate
]

{ #category : '*Deprecated14' }
OpalCompiler >> failBlock [

	^ failBlock
]

{ #category : '*Deprecated14' }
OpalCompiler >> failBlock: anObject [

	failBlock := anObject
]

{ #category : '*Deprecated14' }
OpalCompiler >> noPattern: aBoolean [

	self
		deprecated: 'Use #isScripting: instead'
		transformWith: '`@receiver noPattern: `@arg' -> '`@receiver isScripting: `@arg'.

	self isScripting: aBoolean
]
