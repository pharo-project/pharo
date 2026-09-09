Extension { #name : 'SHTextStyler' }

{ #category : '*PharoDocComment' }
SHTextStyler >> styleDocComment: aComment [
	aComment pharoDocCommentNodes
		do: [ :pharoDocComment |
			self styleDocExpression: pharoDocComment expression in: aComment. ]
]

{ #category : '*PharoDocComment' }
SHTextStyler >> styleDocExpression: aPharoDocExpression in: aComment [

	| expressionText expressionNode off |
	expressionNode := aPharoDocExpression expressionNode.
	expressionText := expressionNode source asText.
	self class new style: expressionText ast: expressionNode.

	off := aComment start.
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
SHTextStyler >> visitMethodComments: aNode [

	PharoDocCommentNode docCommentEnabled ifTrue: [ aNode comments do: [ :comment | self styleDocComment: comment ] ]
]
