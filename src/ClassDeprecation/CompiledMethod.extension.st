Extension { #name : #CompiledMethod }

{ #category : #'*ClassDeprecation' }
CompiledMethod >> hasDeprecationTransformableReferences [
	"Answers whether or not it references any class that is deprecated and transformable."

	^ self referencedClasses anySatisfy: #isDeprecatedTransformable
]

{ #category : #'*ClassDeprecation' }
CompiledMethod >> installDeprecationTransformMetaLinks [
	"Installs the deprecationTransformMetaLinks on applicable nodes in the AST."

	self ast acceptVisitor: ClassDeprecationMetaLinkInstaller new
]

{ #category : #'*ClassDeprecation' }
CompiledMethod >> isDeprecationTransformableMethod [
	"Answers whether or not it is the method defining the deprecation transformation, which is the class-side #isDeprecated method with a #transformTo: pragma."

	^ self selector = #isDeprecated
		and: [ self methodClass isClassSide
				and: [ self hasPragmaNamed: #transformTo: ] ]
]

{ #category : #'*ClassDeprecation' }
CompiledMethod >> uninstallDeprecationTransformMetaLinks [
	"Uninstalls the existing deprecationTransformMetaLinks from the AST."

	self ast acceptVisitor: ClassDeprecationMetaLinkUninstaller new
]
