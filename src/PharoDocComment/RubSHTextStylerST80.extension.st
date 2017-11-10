Extension { #name : #RubSHTextStylerST80 }

{ #category : #'*PharoDocComment' }
RubSHTextStylerST80 >> styleDocComment: aRBComment [
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
RubSHTextStylerST80 >> styleDocExpression: aPharoDocExpression in: aRBComment [
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
RubSHTextStylerST80 >> visitMethodComments: comments [
	comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	PharoDocCommentNode docCommentEnabled
		ifTrue: [ comments do: [ :comment | self styleDocComment: comment ] ]
]

{ #category : #'*PharoDocComment' }
RubSHTextStylerST80 >> visitMethodNode: aMethodNode [
	| link |
	self visitMethodComments: aMethodNode comments.
	aMethodNode arguments do: [ :argument | self addStyle: #patternArg forNode: argument ].
	link := TextMethodLink selector: aMethodNode selector.
	aMethodNode selectorParts
		with: aMethodNode keywordsPositions
		do:
			[ :keyword :position | 
			self
				addStyle: #patternKeyword
				attribute: link
				from: position
				to: position + keyword size - 1 ].
	aMethodNode pragmas do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
]
