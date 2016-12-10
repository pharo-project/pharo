*PharoDocComment
styleDocComment: aRBComment
	aRBComment pharoDocCommentNodes
		do: [ :pharoDocComment | 
			self styleDocExpression: pharoDocComment expression in: aRBComment.
			self
				addStyle: #binary
				from: aRBComment start + pharoDocComment expression expressionInterval last
				to: aRBComment start + pharoDocComment result expressionInterval first.
			self styleDocExpression: pharoDocComment result in: aRBComment ]