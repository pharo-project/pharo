requests
shouldInlineExpression: aString 
	^(self options at: #inlineExpression) value: self value: aString