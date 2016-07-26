initialization
initialize
	useBasicCommentFormat := true.
	formatCommentWithStatements := true.
	indentString := String with: Character tab.
	indentsForKeywords := 1.
	keepBlockInMessage := true.
	lineUpBlockBrackets := false.
	maxLineLength := 70.
	methodSignatureOnMultipleLines := false.
	minimumNewLinesBetweenStatements := 1.
	multiLineMessages := #(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifTrue: #ifFalse: #on:do: #ensure: #ifCurtailed:).
	newLineAfterCascade := true.
	newLineBeforeFirstCascade := true.
	newLineBeforeFirstKeyword := false.
	newLinesAfterMethodComment := 2.
	newLinesAfterMethodPattern := 1.
	newLinesAfterTemporaries := 1.
	numberOfArgumentsForMultiLine := 4.
	oneLineMessages := #(#to: #to:do: #to:by: #to:by:do:).
	periodsAtEndOfBlock := false.
	periodsAtEndOfMethod := false.
	retainBlankLinesBetweenStatements := false.
	retainBlankLinesBeforeComments := true.
	selectorAndArgumentCombinedMaxSize := 40.
	stringFollowingReturn := ' '.
	stringInsideBlocks := ' '.
	stringInsideParentheses := ''.
	traditionalBinaryPrecedenceArray := #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@)).
	useTraditionalBinaryPrecedenceForParentheses := true