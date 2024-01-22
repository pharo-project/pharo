Extension { #name : 'ASTComment' }

{ #category : '*PharoDocComment' }
ASTComment >> hasDocComment [
	^ (self hasProperty: #pharoDocCommentNodes) or: [contents includesSubstring: '>>>']
]

{ #category : '*PharoDocComment' }
ASTComment >> pharoDocCommentNodes [
	self hasDocComment ifFalse: [ ^#() ].
	^ self propertyAt: #pharoDocCommentNodes ifAbsentPut: [PharoDocCommentNode parseDocComments: self]
]
