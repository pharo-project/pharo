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

{ #category : #'*Deprecated90' }
CompiledMethod >> propertyValueAt: propName [

	"the is the old API, we unified the API over all structure (Classes, Methods, AST Nodes)"

	self
		deprecated: 'use #propertyAt:'
		transformWith: '`@receiver propertyValueAt: `@arg' -> '`@receiver propertyAt: `@arg'.
	^ self propertyAt: propName
]

{ #category : #'*Deprecated90' }
CompiledMethod >> propertyValueAt: propName ifAbsent: aBlock [
	"the is the old API, we unified the API over all structure (Classes, Methods, AST Nodes)"
	self
		deprecated: 'use #propertyAt:ifAbsent:'
		transformWith: '`@receiver propertyValueAt: `@arg1 ifAbsent: `@arg2' -> '`@receiver propertyAt: `@arg1 ifAbsent: `@arg2'.
	^self propertyAt: propName ifAbsent: aBlock
]

{ #category : #'*Deprecated90' }
CompiledMethod >> propertyValueAt: propName put: propValue [
	"the is the old API, we unified the API over all structure (Classes, Methods, AST Nodes)"
	self
		deprecated: 'use #propertyAt:put:'
		transformWith: '`@receiver propertyValueAt: `@arg1 put: `@arg2' -> '`@receiver propertyAt: `@arg1 put: `@arg2'.
	^self propertyAt: propName put: propValue
]
