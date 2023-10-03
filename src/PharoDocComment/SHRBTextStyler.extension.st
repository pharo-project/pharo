Extension { #name : 'SHRBTextStyler' }

{ #category : '*PharoDocComment' }
SHRBTextStyler >> styleDocComment: aRBComment [
	aRBComment pharoDocCommentNodes
		do: [ :pharoDocComment |
			self styleDocExpression: pharoDocComment expression in: aRBComment. ]
]

{ #category : '*PharoDocComment' }
SHRBTextStyler >> styleDocExpression: aPharoDocExpression in: aRBComment [

	| expressionText expressionNode off |
	expressionNode := aPharoDocExpression expressionNode.
	expressionText := expressionNode source asText.
	self class new style: expressionText ast: expressionNode.

	off := aRBComment start.
	expressionText withIndexDo: [ :char :ij |
		| index attr |
		index := ij + off.
		attr := expressionText attributesAt: ij.
		charAttr at: index put: attr.
		(expressionText at: ij) = $" ifTrue: [
			"Because doublequote are escaped in the original source code,
			just highlight both character the same and increase the original source offset"
			charAttr at: index + 1 put: attr.
			off := off + 1 ] ]
]

{ #category : '*PharoDocComment' }
SHRBTextStyler >> visitMethodComments: aNode [

	PharoDocCommentNode docCommentEnabled ifTrue: [ aNode comments do: [ :comment | self styleDocComment: comment ] ]
]
