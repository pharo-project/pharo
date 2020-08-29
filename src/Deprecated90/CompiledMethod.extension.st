Extension { #name : #CompiledMethod }

{ #category : #'*Deprecated90' }
CompiledMethod >> actualClass [
	self 
		deprecated: 'use #methodClass'
		transformWith:  '`@receiver actualClass' -> '`@receiver methodClass'.
	^self methodClass
]

{ #category : #'*Deprecated90' }
CompiledMethod >> classIsMeta [
	self 
		deprecated: 'use methodClass isClassSide'
		transformWith:  '`@receiver classIsMeta' -> '`@receiver methodClass isClassSide'.
	^self methodClass isClassSide
]

{ #category : #'*Deprecated90' }
CompiledMethod >> hasLinks [
	self 
		deprecated: 'use #hasMetaLinks'
		transformWith:  '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^self hasMetaLinks
]
