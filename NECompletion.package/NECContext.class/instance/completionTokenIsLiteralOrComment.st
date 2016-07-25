private-type-guessing
completionTokenIsLiteralOrComment
	^ self completionTokenIsLiteral or:[ self completionTokenIsComment]