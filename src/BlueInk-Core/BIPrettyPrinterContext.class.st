"
I represent a set of settings that the BlueInk prettyprinter uses. 
I'm useful to set a prettyPrinter. 
"
Class {
	#name : #BIPrettyPrinterContext,
	#superclass : #Object,
	#instVars : [
		'formatCommentWithStatements',
		'indentString',
		'indentsForKeywords',
		'keepBlockInMessage',
		'lineUpBlockBrackets',
		'maxLineLength',
		'methodSignatureOnMultipleLines',
		'minimumNewLinesBetweenStatements',
		'multiLineMessages',
		'newLineAfterCascade',
		'newLineBeforeFirstCascade',
		'newLineBeforeFirstKeyword',
		'newLinesAfterMethodComment',
		'newLinesAfterMethodPattern',
		'newLinesAfterTemporaries',
		'numberOfArgumentsForMultiLine',
		'oneLineMessages',
		'periodsAtEndOfBlock',
		'periodsAtEndOfMethod',
		'retainBlankLinesBetweenStatements',
		'retainBlankLinesBeforeComments',
		'stringFollowingReturn',
		'stringInsideBlocks',
		'stringInsideParentheses',
		'useTraditionalBinaryPrecedenceForParentheses',
		'selectorAndArgumentCombinedMaxSize',
		'useBasicCommentFormat',
		'traditionalBinaryPrecedenceArray'
	],
	#category : #'BlueInk-Core-Utilities'
}

{ #category : #accessing }
BIPrettyPrinterContext >> formatCommentWithStatements [
	^ formatCommentWithStatements
]

{ #category : #accessing }
BIPrettyPrinterContext >> formatCommentWithStatements: anObject [
	formatCommentWithStatements := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> indentString [
	^ indentString
]

{ #category : #accessing }
BIPrettyPrinterContext >> indentString: aString [
	indentString := aString
]

{ #category : #accessing }
BIPrettyPrinterContext >> indentsForKeywords [
	^ indentsForKeywords
]

{ #category : #accessing }
BIPrettyPrinterContext >> indentsForKeywords: anObject [
	indentsForKeywords := anObject
]

{ #category : #initialization }
BIPrettyPrinterContext >> initialize [
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
]

{ #category : #accessing }
BIPrettyPrinterContext >> keepBlockInMessage [
	^ keepBlockInMessage
]

{ #category : #accessing }
BIPrettyPrinterContext >> keepBlockInMessage: anObject [
	keepBlockInMessage := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> lineUpBlockBrackets [
	^ lineUpBlockBrackets
]

{ #category : #accessing }
BIPrettyPrinterContext >> lineUpBlockBrackets: anObject [
	lineUpBlockBrackets := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> maxLineLength [
	^ maxLineLength
]

{ #category : #accessing }
BIPrettyPrinterContext >> maxLineLength: anObject [
	maxLineLength := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> methodSignatureOnMultipleLines [
	^ methodSignatureOnMultipleLines
]

{ #category : #accessing }
BIPrettyPrinterContext >> methodSignatureOnMultipleLines: anObject [
	methodSignatureOnMultipleLines := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> minimumNewLinesBetweenStatements [
	^ minimumNewLinesBetweenStatements
]

{ #category : #accessing }
BIPrettyPrinterContext >> minimumNewLinesBetweenStatements: anObject [
	minimumNewLinesBetweenStatements := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> multiLineMessages [
	^ multiLineMessages
]

{ #category : #accessing }
BIPrettyPrinterContext >> multiLineMessages: anArray [
	multiLineMessages := (self class compiler evaluate: anArray asString)
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineAfterCascade [
	^ newLineAfterCascade
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineAfterCascade: anObject [
	newLineAfterCascade := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineBeforeFirstCascade [
	^ newLineBeforeFirstCascade
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineBeforeFirstCascade: anObject [
	newLineBeforeFirstCascade := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineBeforeFirstKeyword [
	^ newLineBeforeFirstKeyword
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLineBeforeFirstKeyword: anObject [
	newLineBeforeFirstKeyword := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterMethodComment [
	^ newLinesAfterMethodComment
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterMethodComment: anObject [
	newLinesAfterMethodComment := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterMethodPattern [
	^ newLinesAfterMethodPattern
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterMethodPattern: anObject [
	newLinesAfterMethodPattern := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterTemporaries [
	^ newLinesAfterTemporaries
]

{ #category : #accessing }
BIPrettyPrinterContext >> newLinesAfterTemporaries: anObject [
	newLinesAfterTemporaries := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> numberOfArgumentsForMultiLine [
	^ numberOfArgumentsForMultiLine
]

{ #category : #accessing }
BIPrettyPrinterContext >> numberOfArgumentsForMultiLine: anObject [
	numberOfArgumentsForMultiLine := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> oneLineMessages [
	^ oneLineMessages
]

{ #category : #accessing }
BIPrettyPrinterContext >> oneLineMessages: anArray [
	oneLineMessages := (self class compiler evaluate: anArray asString)
]

{ #category : #accessing }
BIPrettyPrinterContext >> periodsAtEndOfBlock [
	^ periodsAtEndOfBlock
]

{ #category : #accessing }
BIPrettyPrinterContext >> periodsAtEndOfBlock: anObject [
	periodsAtEndOfBlock := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> periodsAtEndOfMethod [
	^ periodsAtEndOfMethod
]

{ #category : #accessing }
BIPrettyPrinterContext >> periodsAtEndOfMethod: anObject [
	periodsAtEndOfMethod := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> retainBlankLinesBeforeComments [
	^ retainBlankLinesBeforeComments
]

{ #category : #accessing }
BIPrettyPrinterContext >> retainBlankLinesBeforeComments: anObject [
	retainBlankLinesBeforeComments := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> retainBlankLinesBetweenStatements [
	^ retainBlankLinesBetweenStatements
]

{ #category : #accessing }
BIPrettyPrinterContext >> retainBlankLinesBetweenStatements: anObject [
	retainBlankLinesBetweenStatements := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> selectorAndArgumentCombinedMaxSize [
	^ selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
BIPrettyPrinterContext >> selectorAndArgumentCombinedMaxSize: anInteger [
	selectorAndArgumentCombinedMaxSize := anInteger
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringFollowingReturn [
	^ stringFollowingReturn
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringFollowingReturn: anObject [
	stringFollowingReturn := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringInsideBlocks [
	^ stringInsideBlocks
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringInsideBlocks: anObject [
	stringInsideBlocks := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringInsideParentheses [
	^ stringInsideParentheses
]

{ #category : #accessing }
BIPrettyPrinterContext >> stringInsideParentheses: anObject [
	stringInsideParentheses := anObject
]

{ #category : #accessing }
BIPrettyPrinterContext >> traditionalBinaryPrecedenceArray [
	^ traditionalBinaryPrecedenceArray
]

{ #category : #accessing }
BIPrettyPrinterContext >> traditionalBinaryPrecedenceArray: anArray [
	traditionalBinaryPrecedenceArray := (self class compiler evaluate: anArray asString)
]

{ #category : #accessing }
BIPrettyPrinterContext >> useBasicCommentFormat [
	^ useBasicCommentFormat
]

{ #category : #accessing }
BIPrettyPrinterContext >> useBasicCommentFormat: aBoolean [
	useBasicCommentFormat := aBoolean
]

{ #category : #accessing }
BIPrettyPrinterContext >> useTraditionalBinaryPrecedenceForParentheses [
	^ useTraditionalBinaryPrecedenceForParentheses
]

{ #category : #accessing }
BIPrettyPrinterContext >> useTraditionalBinaryPrecedenceForParentheses: anObject [
	useTraditionalBinaryPrecedenceForParentheses := anObject
]
