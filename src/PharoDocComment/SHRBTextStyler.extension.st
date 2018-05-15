Extension { #name : #SHRBTextStyler }

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> styleDocComment: aRBComment [
	aRBComment pharoDocCommentNodes
		do: [ :pharoDocComment | 
			self styleDocExpression: pharoDocComment expression in: aRBComment.
			self
				addStyle: #binary
				from: aRBComment start + pharoDocComment expression expressionInterval last
				to: aRBComment start + pharoDocComment result expressionInterval first.
			self styleDocExpression: pharoDocComment result in: aRBComment ]
]

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> styleDocExpression: aPharoDocExpression in: aRBComment [
	| expressionText expressionNode expressionInterval |
	expressionNode := aPharoDocExpression expressionCode.
	expressionText := expressionNode source asText.
	expressionInterval := aPharoDocExpression expressionInterval.
	self class new style: expressionText ast: expressionNode.
	expressionText
		withIndexDo: [ :char :ij | 
			| index |
			index := ij - 1 + aRBComment start.
			charAttr from: expressionInterval first + index to: expressionInterval first + index put: (expressionText attributesAt: ij) ]
]

{ #category : #'*PharoDocComment' }
SHRBTextStyler >> visitMethodComments: comments [
	comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	PharoDocCommentNode docCommentEnabled
		ifTrue: [ comments do: [ :comment | self styleDocComment: comment ] ]
]
