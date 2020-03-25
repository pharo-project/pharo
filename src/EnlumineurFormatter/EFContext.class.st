"
I represent all the parameters that can modify a pretty printer. 
Now since the BlueInkPrettyPrinter context was too complex to refactor and extend without breaking everything we clone it
with the related tests. 

The goal is to make sure that we can have a better prettyprinter and replace the Basic pretty printer (configurationFormatter)
"
Class {
	#name : #EFContext,
	#superclass : #Object,
	#instVars : [
		'formatCommentWithStatements',
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
		'newLinesAfterTemporaries',
		'numberOfArgumentsForMultiLine',
		'oneLineMessages',
		'retainBlankLinesBetweenStatements',
		'retainBlankLinesBeforeComments',
		'selectorAndArgumentCombinedMaxSize',
		'useBasicCommentFormat',
		'traditionalBinaryPrecedenceArray',
		'numberOfSpacesInsideBlock',
		'numberOfSpacesInsideParentheses',
		'numberOfSpacesAfterCaretSymbolInReturn',
		'numberOfSpacesInIndent',
		'spacesAfterCaretSymbolInReturn',
		'spacesInsideBlocks',
		'indentStyle',
		'indentString',
		'spacesInsideParentheses',
		'indentExtraSpaces',
		'spacesInsideArray',
		'numberOfSpacesInsideArray',
		'numberOfSpacesBeforeDotInDynamicArray',
		'spacesBeforeDotInDynamicArray',
		'numberOfNewLinesAfterMethodComment',
		'newLinesAfterMethodSignature',
		'periodAtEndOfMethod',
		'periodAtEndOfBlock'
	],
	#category : #'EnlumineurFormatter-Core'
}

{ #category : #initialization }
EFContext >> configureMultiLineMessages [

	multiLineMessages := self defaultMultiLineMessages
]

{ #category : #initialization }
EFContext >> configureOneLineMessages [
	oneLineMessages := self defaultOneLineMessages
]

{ #category : #initialization }
EFContext >> defaultMultiLineMessages [

	^ #(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifTrue: #ifFalse: #on:do: #ensure: #ifCurtailed:)
]

{ #category : #initialization }
EFContext >> defaultOneLineMessages [
  ^#(#to: #to:do: #to:by: #to:by:do:)
]

{ #category : #accessing }
EFContext >> formatCommentCloseToStatements [
	^ formatCommentWithStatements
]

{ #category : #accessing }
EFContext >> formatCommentCloseToStatements: aBoolean [
	formatCommentWithStatements := aBoolean
]

{ #category : #accessing }
EFContext >> indentExtraSpaces [
	^ indentExtraSpaces
]

{ #category : #accessing }
EFContext >> indentExtraSpaces: anInteger [
	"To set the number of spaces that represent an indent"

	indentExtraSpaces := self stringFromSpaceNumber: anInteger
]

{ #category : #accessing }
EFContext >> indentString [
	"My value is set using numberOfSpacesInIndent: "
	
	^ indentString
]

{ #category : #accessing }
EFContext >> indentStyle [
	^ indentStyle
]

{ #category : #accessing }
EFContext >> indentStyle: aSymbol [
	"Indent style can be either #tabulation or #space."
	
	indentStyle := aSymbol.
	indentString := aSymbol = #space
		ifTrue: [ self stringFromSpaceNumber: numberOfSpacesInIndent ]
		ifFalse: [ String tab ]
]

{ #category : #accessing }
EFContext >> indentsForKeywords [
	^ indentsForKeywords
]

{ #category : #accessing }
EFContext >> indentsForKeywords: anInteger [
	indentsForKeywords := anInteger
]

{ #category : #initialization }
EFContext >> initialize [
  super initialize.
  self useBasicCommentFormat: true.
  self formatCommentCloseToStatements: true.
  indentsForKeywords := 1.
  keepBlockInMessage := true.
  lineUpBlockBrackets := false.
  maxLineLength := 70.
  methodSignatureOnMultipleLines := false.
  minimumNewLinesBetweenStatements := 1.
  self configureMultiLineMessages.
  newLineAfterCascade := true.
  newLineBeforeFirstCascade := true.
  newLineBeforeFirstKeyword := true.
  numberOfNewLinesAfterMethodComment := 2.
  newLinesAfterMethodSignature := 1.
  newLinesAfterTemporaries := 1.
  numberOfArgumentsForMultiLine := 4.
  self configureOneLineMessages.
  self periodAtEndOfBlock: false.
  self periodAtEndOfMethod: false.
  self retainBlankLinesBetweenStatements: false.
  self retainBlankLinesBeforeComments: true.
  self selectorAndArgumentCombinedMaxSize: 40.
  self numberOfSpacesInsideBlock: 1.
  self numberOfSpacesInsideParentheses: 0.
  self numberOfSpacesAfterCaretSymbolInReturn: 1.
  self indentStyle: #tabulation.
  self numberOfSpacesInIndent: 3.
  self numberOfSpacesBeforeDotInDynamicArray: 0.
  self indentExtraSpaces: 0.
  self numberOfSpacesInsideArray: 1
]

{ #category : #accessing }
EFContext >> keepBlockInMessage [
	^ keepBlockInMessage
]

{ #category : #accessing }
EFContext >> keepBlockInMessage: aBoolean [
	keepBlockInMessage := aBoolean
]

{ #category : #accessing }
EFContext >> lineUpBlockBrackets [
	"When true place a new line before closing a multiline block.
	1 to: 4 do: [ Transcript
		show: i;
		cr
	]
	"
	
	^ lineUpBlockBrackets
]

{ #category : #accessing }
EFContext >> lineUpBlockBrackets: aBoolean [
	lineUpBlockBrackets := aBoolean
]

{ #category : #accessing }
EFContext >> maxLineLength [
	^ maxLineLength
]

{ #category : #accessing }
EFContext >> maxLineLength: anInteger [
	maxLineLength := anInteger
]

{ #category : #accessing }
EFContext >> methodSignatureOnMultipleLines [
	^ methodSignatureOnMultipleLines
]

{ #category : #accessing }
EFContext >> methodSignatureOnMultipleLines: aBoolean [
	methodSignatureOnMultipleLines := aBoolean
]

{ #category : #accessing }
EFContext >> minimumNewLinesBetweenStatements [
	^ minimumNewLinesBetweenStatements
]

{ #category : #accessing }
EFContext >> minimumNewLinesBetweenStatements: anInteger [
	minimumNewLinesBetweenStatements := anInteger
]

{ #category : #accessing }
EFContext >> multiLineMessages [
	^ multiLineMessages
]

{ #category : #accessing }
EFContext >> multiLineMessages: anArray [
	"In case the user entered a string representing array we convert it into array."
	self flag: #tocheck. "we should change this logic. It is terrible, the UI should give us an array and not a string!"
	multiLineMessages := self class compiler evaluate: anArray asString
]

{ #category : #accessing }
EFContext >> newLineAfterCascade [
	^ newLineAfterCascade
]

{ #category : #accessing }
EFContext >> newLineAfterCascade: anInteger [
	newLineAfterCascade := anInteger
]

{ #category : #accessing }
EFContext >> newLineBeforeFirstCascade [
	^ newLineBeforeFirstCascade
]

{ #category : #accessing }
EFContext >> newLineBeforeFirstCascade: anInteger [
	newLineBeforeFirstCascade := anInteger
]

{ #category : #accessing }
EFContext >> newLineBeforeFirstKeyword [
	^ newLineBeforeFirstKeyword
]

{ #category : #accessing }
EFContext >> newLineBeforeFirstKeyword: aBoolean [
	newLineBeforeFirstKeyword := aBoolean
]

{ #category : #accessing }
EFContext >> numberOfArgumentsForMultiLine [
	^ numberOfArgumentsForMultiLine
]

{ #category : #accessing }
EFContext >> numberOfArgumentsForMultiLine: anInteger [
	numberOfArgumentsForMultiLine := anInteger
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterMethodComment [
	^ numberOfNewLinesAfterMethodComment
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterMethodComment: aBoolean [
	numberOfNewLinesAfterMethodComment := aBoolean
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterMethodSignature [
	^ newLinesAfterMethodSignature
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterMethodSignature: aBoolean [
	newLinesAfterMethodSignature := aBoolean
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterTemporaries [
	^ newLinesAfterTemporaries
]

{ #category : #accessing }
EFContext >> numberOfNewLinesAfterTemporaries: anInteger [
	newLinesAfterTemporaries := anInteger
]

{ #category : #accessing }
EFContext >> numberOfSpacesAfterCaretSymbolInReturn [
	^ numberOfSpacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFContext >> numberOfSpacesAfterCaretSymbolInReturn: aNumber [
	spacesAfterCaretSymbolInReturn := self stringFromSpaceNumber: aNumber.
	numberOfSpacesAfterCaretSymbolInReturn := aNumber
]

{ #category : #accessing }
EFContext >> numberOfSpacesBeforeDotInDynamicArray [
	^ numberOfSpacesBeforeDotInDynamicArray
]

{ #category : #accessing }
EFContext >> numberOfSpacesBeforeDotInDynamicArray: anInteger [

	spacesBeforeDotInDynamicArray := self stringFromSpaceNumber: anInteger.
	numberOfSpacesBeforeDotInDynamicArray := anInteger
]

{ #category : #accessing }
EFContext >> numberOfSpacesInIndent [
	^ numberOfSpacesInIndent
]

{ #category : #accessing }
EFContext >> numberOfSpacesInIndent: anInteger [
	numberOfSpacesInIndent := anInteger.
	indentString := self indentStyle = #space 
		ifTrue: [ self stringFromSpaceNumber: anInteger ]
		ifFalse: [ String tab ]
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideArray [
	^ numberOfSpacesInsideArray
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideArray: anInteger [
	spacesInsideArray := self stringFromSpaceNumber: anInteger.
	numberOfSpacesInsideArray := anInteger
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideBlock [
	^ numberOfSpacesInsideBlock
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideBlock: aNumber [
	spacesInsideBlocks := self stringFromSpaceNumber: aNumber.
	numberOfSpacesInsideBlock := aNumber
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideParentheses [
	^ numberOfSpacesInsideParentheses
]

{ #category : #accessing }
EFContext >> numberOfSpacesInsideParentheses: aNumber [
	spacesInsideParentheses := self stringFromSpaceNumber: aNumber.
	numberOfSpacesInsideParentheses := aNumber
]

{ #category : #accessing }
EFContext >> oneLineMessages [
	^ oneLineMessages
]

{ #category : #accessing }
EFContext >> oneLineMessages: anArray [
	"To be revisited latter. Apparently this trick is to convert strings into an array."
	oneLineMessages := self class compiler evaluate: anArray asString
]

{ #category : #accessing }
EFContext >> periodAtEndOfBlock: aBoolean [
	periodAtEndOfBlock := aBoolean
]

{ #category : #accessing }
EFContext >> periodAtEndOfMethod: aBoolean [
	periodAtEndOfMethod := aBoolean
]

{ #category : #accessing }
EFContext >> periodsAtEndOfBlock [
	^ periodAtEndOfBlock
]

{ #category : #accessing }
EFContext >> periodsAtEndOfMethod [
	^ periodAtEndOfMethod
]

{ #category : #accessing }
EFContext >> retainBlankLinesBeforeComments [
	^ retainBlankLinesBeforeComments
]

{ #category : #accessing }
EFContext >> retainBlankLinesBeforeComments: aBoolean [
	retainBlankLinesBeforeComments := aBoolean
]

{ #category : #accessing }
EFContext >> retainBlankLinesBetweenStatements [
	^ retainBlankLinesBetweenStatements
]

{ #category : #accessing }
EFContext >> retainBlankLinesBetweenStatements: aBoolean [
	retainBlankLinesBetweenStatements := aBoolean
]

{ #category : #accessing }
EFContext >> selectorAndArgumentCombinedMaxSize [
	^ selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
EFContext >> selectorAndArgumentCombinedMaxSize: anInteger [
	selectorAndArgumentCombinedMaxSize := anInteger
]

{ #category : #accessing }
EFContext >> spacesAfterCaretSymbolInReturnString [
	^ spacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFContext >> spacesBeforeDotInDynamicArray [ 
	^ spacesBeforeDotInDynamicArray 
]

{ #category : #accessing }
EFContext >> spacesInsideArray [
	^ spacesInsideArray
]

{ #category : #accessing }
EFContext >> spacesInsideBlocksString [
	^ spacesInsideBlocks
]

{ #category : #accessing }
EFContext >> spacesInsideParenthesesString [
	^ spacesInsideParentheses
]

{ #category : #printing }
EFContext >> storeOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' new';
		cr.
	self class instVarNames sorted
		do: [ :each | 
			aStream tab;
				nextPutAll: each;
				nextPutAll: ': '.
			aStream print: (self instVarNamed: each asSymbol) ]
		separatedBy: [ aStream
				nextPutAll: ';';
				cr ]
]

{ #category : #accessing }
EFContext >> stringFromSpaceNumber: aNumber [
	
	^ String new: aNumber withAll: Character space
]

{ #category : #accessing }
EFContext >> useBasicCommentFormat [
	^ useBasicCommentFormat
]

{ #category : #accessing }
EFContext >> useBasicCommentFormat: aBoolean [
	useBasicCommentFormat := aBoolean
]
