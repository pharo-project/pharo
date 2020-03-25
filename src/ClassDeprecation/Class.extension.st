Extension { #name : #Class }

{ #category : #'*ClassDeprecation' }
Class >> deprecationTransformationExpression [
	"Answers the expression a class reference should be transformed to. Can be any kind of expression, e.g. another class name or a method send."

	^ ((self class compiledMethodAt: #isDeprecated)
		pragmaAt: #transformTo:) arguments first
]

{ #category : #'*ClassDeprecation' }
Class >> installDeprecationTransformMetaLinks [
	"Installs the deprecationTransformMetaLinks on all methods referencing it."

	self usingMethods
		do: [ :e | e compiledMethod installDeprecationTransformMetaLinks ]
]

{ #category : #'*ClassDeprecation' }
Class >> isDeprecatedTransformable [
	"Answers whether or not it can be transformed with a deprecation transformation. I.e. it has a class-side #isDeprecated method with a #transformTo: pragma."

	^ (self class includesSelector: #isDeprecated)
		and: [ ((self class compiledMethodAt: #isDeprecated)
				hasPragmaNamed: #transformTo:) and: [ self isDeprecated ] ]
]

{ #category : #'*ClassDeprecation' }
Class >> uninstallDeprecationTransformMetaLinks [
	"Uninstalls the deprecationTransformMetaLinks from all methods referencing it."

	self usingMethods
		do: [ :e | e compiledMethod uninstallDeprecationTransformMetaLinks ]
]
