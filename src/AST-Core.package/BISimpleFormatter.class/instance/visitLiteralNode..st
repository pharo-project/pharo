visiting
visitLiteralNode: aLiteralNode
	aLiteralNode value isLiteral
		ifFalse: [ 

			self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString: aLiteralNode sourceText ]