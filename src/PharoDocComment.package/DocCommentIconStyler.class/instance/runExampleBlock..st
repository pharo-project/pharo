private
runExampleBlock: aNode
	^ [ | examples |
		examples := aNode pharoDocCommentNodes.
		(examples
			collect: [ :example | 
			{ example expression expressionCode formattedCode . example expression evaluate . example result expressionCode formattedCode}]) inspect]