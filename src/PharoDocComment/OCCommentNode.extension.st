Extension { #name : 'OCCommentNode' }

{ #category : '*PharoDocComment' }
OCCommentNode >> hasDocComment [
	^ (self hasProperty: #pharoDocCommentNodes) or: [contents includesSubstring: '>>>']
]

{ #category : '*PharoDocComment' }
OCCommentNode >> pharoDocCommentNodes [
	self hasDocComment ifFalse: [ ^#() ].
	^ self propertyAt: #pharoDocCommentNodes ifAbsentPut: [PharoDocCommentNode parseDocComments: self]
]
