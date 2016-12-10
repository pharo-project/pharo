*PharoDocComment
styleDocExpression: aPharoDocExpression in: aRBComment
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