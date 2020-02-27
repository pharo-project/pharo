Extension { #name : #RBGlobalNode }

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> deprecationTransform [
	"Replaces itself with the deprecationTransformExpression of the refererred class."

	| rule class |
	self assert: [ self isDeprecatedTransformable ].
	class := self binding value.
	rule := RBParseTreeRewriter new
		replace: class name
		with: class deprecationTransformationExpression.
	^ (rule executeTree: self)
		ifTrue: [ | tree |
			tree := rule tree.
			self replaceWith: tree.
			tree ]
		ifFalse: [ self flag: 'TODO should we log something if nothing is done?'.
			self ]
]

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> deprecationTransformMetaLink [
	"Answers its deprecationTransformMetaLink, if any."

	^ self links detect: [ :e | e isKindOf: ClassDeprecationMetaLink ]
]

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> hasDeprecationTransformMetaLink [
	"Answers whether or not it has a deprecationTransformMetaLink."

	^ self links
		anySatisfy: [ :e | e isKindOf: ClassDeprecationMetaLink ]
]

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> installDeprecationTransformMetaLink [
	"Installs a deprecationTransformMetaLink, if not already installed. Does nothing otherwise."

	self hasDeprecationTransformMetaLink
		ifFalse: [ self link: ClassDeprecationMetaLink new ]
]

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> isDeprecatedTransformable [
	"Answers whether or not it can be transformed with a deprecation transformation. Currently, only class references can be transformed."

	^ self binding isGlobalClassNameBinding
		and: [ self binding value isDeprecatedTransformable ]
]

{ #category : #'*ClassDeprecation' }
RBGlobalNode >> uninstallDeprecationTransformMetaLink [
	"Uninstalls the deprecationTransformMetaLink, if already installed. Does nothing otherwise."

	self hasDeprecationTransformMetaLink
		ifTrue: [ self removeLink: self deprecationTransformMetaLink ]
]
