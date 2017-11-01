Extension { #name : #RBComment }

{ #category : #'*PharoDocComment' }
RBComment >> pharoDocCommentNodes [
	^ PharoDocCommentNode parseDocComments: self
]
