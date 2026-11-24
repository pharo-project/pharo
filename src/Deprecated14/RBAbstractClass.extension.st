Extension { #name : 'RBAbstractClass' }

{ #category : '*Deprecated14' }
RBAbstractClass >> classBinding [
	self
		deprecated: 'Use binding'
		in: #Pharo14
		transformWith: '`@receiver classBinding' -> '`@receiver binding'.
	^ self binding 
]

{ #category : '*Deprecated14' }
RBAbstractClass >> parseTreeFor: aSymbol [

	self
		deprecated: 'Use parseTreeForSelector: instead'
		in: #Pharo14
		transformWith: '`@receiver parseTreeFor: `@argument'
			-> '`@receiver parseTreeForSelector: `@argument'.
	^ self parseTreeForSelector: aSymbol
]

{ #category : '*Deprecated14' }
RBAbstractClass >> theMetaClass [
	self
		deprecated: 'Please use #classSide instead'
		in: #Pharo14
		transformWith: '`@receiver theMetaClass'
						-> '`@receiver classSide'.
	^ self classSide
]

{ #category : '*Deprecated14' }
RBAbstractClass >> theNonMetaClass [
	self
		deprecated: 'Please use #instanceSide instead'
		in: #Pharo14
		transformWith: '`@receiver theNonMetaClass'
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : '*Deprecated14' }
RBAbstractClass >> whoDefinesMethod: aSelector [

	self
		deprecated: 'Use whichClassIncludesSelector: instead of current one'
		in: #Pharo14
		transformWith: '`@rec whoDefinesMethod: `@arg' -> '`@rec whichClassIncludesSelector: `@arg'.
	^ self whichClassIncludesSelector: aSelector 
]
