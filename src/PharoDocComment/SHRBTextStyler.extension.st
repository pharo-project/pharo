Extension { #name : #SHRBTextStyler }

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> styleDocComment: aRBComment [
	aRBComment pharoDocCommentNodes
		do: [ :pharoDocComment | 
			self styleDocExpression: pharoDocComment expression in: aRBComment. ]
]

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> styleDocExpression: aPharoDocExpression in: aRBComment [
	| expressionText expressionNode |
	expressionNode := aPharoDocExpression expressionCode.
	expressionText := expressionNode source asText.
	self class new style: expressionText ast: expressionNode.
	expressionText
		withIndexDo: [ :char :ij | 
			| index |
			index := ij + aRBComment start.
			charAttr at: index put: (expressionText attributesAt: ij) ]
]

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> visitMethodComments: comments [
	comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	PharoDocCommentNode docCommentEnabled
		ifTrue: [ comments do: [ :comment | self styleDocComment: comment ] ]
]
