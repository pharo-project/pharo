Extension { #name : #RBComment }

{ #category : #'*PharoDocComment' }
RBComment >> hasDocComment [
	^ (self hasProperty: #pharoDocCommentNodes) or: [contents includesSubstring: '>>>']
]

{ #category : #'*PharoDocComment' }
RBComment >> pharoDocCommentNodes [
	self hasDocComment ifFalse: [ ^#() ].
	^ self propertyAt: #pharoDocCommentNodes ifAbsentPut: [PharoDocCommentNode parseDocComments: self]
]
