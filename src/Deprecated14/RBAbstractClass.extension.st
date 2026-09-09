Extension { #name : 'RBAbstractClass' }

{ #category : '*Deprecated14' }
RBAbstractClass >> classBinding [
	self deprecated: 'Use binding' transformWith: '`@receiver classBinding' -> '`@receiver binding'.
	^ self binding 
]

{ #category : '*Deprecated14' }
RBAbstractClass >> parseTreeFor: aSymbol [

	self
		deprecated: 'Use parseTreeForSelector: instead'
		transformWith: '`@receiver parseTreeFor: `@argument'
			-> '`@receiver parseTreeForSelector: `@argument'.
	^ self parseTreeForSelector: aSymbol
]

{ #category : '*Deprecated14' }
RBAbstractClass >> theMetaClass [
	self deprecated:  'Please use #classSide instead' transformWith:  '`@receiver theMetaClass'
						-> '`@receiver classSide'.
	^ self classSide
]

{ #category : '*Deprecated14' }
RBAbstractClass >> theNonMetaClass [
	self deprecated:  'Please use #instanceSide instead' transformWith:  '`@receiver theNonMetaClass'
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : '*Deprecated14' }
RBAbstractClass >> whoDefinesMethod: aSelector [

	self
		deprecated: 'Use whichClassIncludesSelector: instead of current one'
		transformWith: '`@rec whoDefinesMethod: `@arg' -> '`@rec whichClassIncludesSelector: `@arg'.
	^ self whichClassIncludesSelector: aSelector 
]
