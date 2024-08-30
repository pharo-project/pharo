Extension { #name : 'ASTCommentNode' }

{ #category : '*PharoDocComment' }
ASTCommentNode >> hasDocComment [
	^ (self hasProperty: #pharoDocCommentNodes) or: [contents includesSubstring: '>>>']
]

{ #category : '*PharoDocComment' }
ASTCommentNode >> pharoDocCommentNodes [
	self hasDocComment ifFalse: [ ^#() ].
	^ self propertyAt: #pharoDocCommentNodes ifAbsentPut: [PharoDocCommentNode parseDocComments: self]
]
