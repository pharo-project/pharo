option
shouldInlineExpression: aString
	^ self confirm: ('Do you want to inline the expression ''<1s>'' in the current method?' expandMacrosWith: aString)