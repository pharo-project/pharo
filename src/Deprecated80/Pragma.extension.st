Extension { #name : #Pragma }

{ #category : #'*Deprecated80' }
Pragma >> keyword [
	self
		deprecated: 'Use #selector instead.'
		transformWith: '`@receiver keyword' -> '`@receiver selector'.
	^ self selector
]

{ #category : #'*Deprecated80' }
Pragma class >> keyword: aSymbol arguments: anArray [
	self
		deprecated: 'Use #selector:arguments: instead.'
		transformWith: '`@receiver keyword: `@arg1 arguments: `@arg2' -> '`@receiver selector: `@arg1 arguments: `@arg2'.
	
	^ self selector: aSymbol arguments: anArray
]

{ #category : #'*Deprecated80' }
Pragma >> setArguments: anArray [
	self
		deprecated: 'Use #arguments: instead.'
		transformWith: '`@receiver setArguments: `@arg' -> '`@receiver arguments: `@arg'.
	self arguments: anArray
]

{ #category : #'*Deprecated80' }
Pragma >> setKeyword: aSymbol [
	self
		deprecated: 'Use #selector: instead.'
		transformWith: '`@receiver setKeyword: `@arg' -> '`@receiver selector: `@arg'.
	self selector: aSymbol
]

{ #category : #'*Deprecated80' }
Pragma >> setMethod: aCompiledMethod [
	self
		deprecated: 'Use #method: instead.'
		transformWith: '`@receiver setMethod: `@arg' -> '`@receiver method: `@arg'.
	self method: aCompiledMethod
]
