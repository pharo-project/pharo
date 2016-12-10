private
exampleIsFaulty: aNode
	^ aNode comments
		anySatisfy: [ :commentNode | commentNode pharoDocCommentNodes anySatisfy: [ :node |
				node expression expressionCode isFaulty ]]