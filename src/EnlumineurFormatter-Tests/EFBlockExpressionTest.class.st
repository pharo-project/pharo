Class {
	#name : #EFBlockExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFBlockExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentStyle: #tabulation;
		keepBlockInMessage: true;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFBlockExpressionTest >> blockWithTemporariesConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries: 1;
		indentExtraSpaces:0
]

{ #category : #helpers }
EFBlockExpressionTest >> closeBracket [
	^']'
]

{ #category : #configurations }
EFBlockExpressionTest >> configurationWithThreeSpaces [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 3;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:false;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> doNotKeepBlockInMessageConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentStyle: #tabulation;
		keepBlockInMessage: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentCloseToStatements: true;
		maxLineLength: 50;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFBlockExpressionTest >> dontFormatCommentWithStatementConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:false;
		maxLineLength:50;
		retainBlankLinesBeforeComments: false;
		useBasicCommentFormat: false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> formatCommentWithStatementConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		retainBlankLinesBeforeComments: false;
		useBasicCommentFormat: false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFBlockExpressionTest >> fourSpaceIndentConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInIndent: 4;
		indentStyle: #space;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFBlockExpressionTest >> multiLineConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:false;
		retainBlankLinesBetweenStatements:false;
		retainBlankLinesBeforeComments:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		useBasicCommentFormat:false;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFBlockExpressionTest >> multiLineWithThreeSpacesInsideBlockConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:3;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:false;
		retainBlankLinesBetweenStatements:false;
		retainBlankLinesBeforeComments:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		useBasicCommentFormat:false;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFBlockExpressionTest >> newLineBeforeEndBracketConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: true;
		indentStyle: #tabulation;
		keepBlockInMessage: true;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentCloseToStatements: true;
		maxLineLength: 50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> noNewLineBeforeEndBracketConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentStyle: #tabulation;
		keepBlockInMessage: true;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentCloseToStatements: true;
		maxLineLength: 50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> numberOfSpaceChangedAfterIndentCharacterConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInIndent: 1;
		indentStyle: #space;
		numberOfSpacesInIndent: 2;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> oneSpaceIndentConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInIndent: 1;
		indentStyle: #space;
		indentExtraSpaces:0
]

{ #category : #helpers }
EFBlockExpressionTest >> openBracket [
	^ '['
]

{ #category : #helpers }
EFBlockExpressionTest >> parserClass [
	^ RBParser
]

{ #category : #configurations }
EFBlockExpressionTest >> periodAtEndConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		numberOfSpacesInsideBlock:1;
		periodAtEndOfBlock: true;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentStyle: #tabulation;
		keepBlockInMessage: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentCloseToStatements: true;
		maxLineLength: 50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> tabIndentConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		indentStyle: #tabulation;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFBlockExpressionTest >> tabIndentIsnotAffectedConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages:#(#to:do:);
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		indentStyle: #tabulation;
		numberOfSpacesInIndent: 37;
		indentExtraSpaces:0
]

{ #category : #tests }
EFBlockExpressionTest >> testArgumentIsFormated [
	self validate: ':i|i' isFormattedAs: ' :i | i ' 
]

{ #category : #tests }
EFBlockExpressionTest >> testArgumentsAreFormated [
	
	self 
		validate:':i :j| i '
		isFormattedAs:' :i :j | i '.
]

{ #category : #tests }
EFBlockExpressionTest >> testArgumentsComments [
	
	self 
		validate:':i "aComment" :j "anotherComment"| i '
		isFormattedAs:
' :i "aComment" :j "anotherComment" | i '
]

{ #category : #tests }
EFBlockExpressionTest >> testDoNotKeepBlockInMessageConfiguration [
	
	self validate: ':i | i to: 10 do: [:j | Transcript show:i. Transcript show: j]'
		isFormattedAs: ' :i | 
i to: 10 do:
	[ :j | 
	Transcript show: i.
	Transcript show: j ] '
	with: #doNotKeepBlockInMessageConfiguration.
]

{ #category : #tests }
EFBlockExpressionTest >> testDontFormatCommentWithStatement [
	self
		validate: '1 factorial. "aComment" 2 factorial'
		isFormattedAs:
			' 
1 factorial "aComment".
2 factorial '
		with: #dontFormatCommentWithStatementConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testEndPeriodInside [
	
	self 
		validate:'2.'
		isFormattedAs:' 2. '
		with: #periodAtEndConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testFormatBody [
	self
		validate: ' [1]. [ a:=1] '
		isFormattedAs:
			' 
[ 1 ].
[ a := 1 ] '
]

{ #category : #tests }
EFBlockExpressionTest >> testFormatCommentWithStatement [
	self
		validate: '1 factorial. "aComment" 2 factorial'
		isFormattedAs:
			' 
1 factorial. "aComment"
2 factorial '
		with: #formatCommentWithStatementConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testFourSpaceIndent [
	self
		validate: ':i | i to: 10 do: [:j | Transcript show:i; show: j]'
		isFormattedAs:
' :i | 
i to: 10 do: [ :j | 
    Transcript
        show: i;
        show: j ] '
		with: #fourSpaceIndentConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testLongMultipleArguments [
	self skip.
	self 
		validate: ' :superlong :longlong :long | Transcript show: longlong ; show: long'
		isFormattedAs: 
' :superlong :longlong :long | 
Transcript
	show: longlong;
	show: long '
		with: #numberOfSpaceChangedAfterIndentCharacterConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testLongSingleArgument [
	self skip.
	self 
		validate: ' [:superlonglonglong | Transcript show:i; show: j]'
		isFormattedAs: 
'[ :superlonglonglong | 
	Transcript show:i; show: j]'
		with: #numberOfSpaceChangedAfterIndentCharacterConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testMultiline [

	self 
		validate: '1+3. 6 factorial '
		isFormattedAs: 
' 
1 + 3.
6 factorial '
]

{ #category : #tests }
EFBlockExpressionTest >> testNewLineAfterFirstBracketWhenMultilineWithArguments [

	self 
		validate: ':i | i+3. i factorial '
		isFormattedAs: 
' :i | 
i + 3.
i factorial '
]

{ #category : #tests }
EFBlockExpressionTest >> testNewLineAfterFirstBracketWhenMultilineWithTemporaries [

	self 
		validate: '| tmp | 1+3. 6 factorial '
		isFormattedAs: 
' 
| tmp |
1 + 3.
6 factorial '
with: #blockWithTemporariesConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testNewLineBeforeEndBrackets [

	self 
		validate:'3. 4'  
		isFormattedAs:
' 
3.
4
'  		with: #newLineBeforeEndBracketConfiguration.
]

{ #category : #tests }
EFBlockExpressionTest >> testNoEndPeriodInside [

	self validate: '2.' isFormattedAs: ' 2 '
]

{ #category : #tests }
EFBlockExpressionTest >> testNoExtraSpaceAroundBlock [

	| expr source |
	expr := self parserClass parseExpression: '   ', self openBracket, '2', self closeBracket, '   '.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: self openBracket, ' 2 ', self closeBracket
]

{ #category : #tests }
EFBlockExpressionTest >> testNoExtraSpaceInside [

	self validate: '    2     ' isFormattedAs: ' 2 '
]

{ #category : #tests }
EFBlockExpressionTest >> testNoNewLineBeforeEndBrackets [

	self 
		validate: '3. 4'  
		isFormattedAs:
' 
3.
4 '  with: #noNewLineBeforeEndBracketConfiguration.
]

{ #category : #tests }
EFBlockExpressionTest >> testNotTooLongBlockBody [
	self validate: 'short' isFormattedAs: ' short ' with: #tooLongStatementConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testNumberOfSpaces [

	self validate: '2' isFormattedAs: '   2   ' with: #configurationWithThreeSpaces
]

{ #category : #tests }
EFBlockExpressionTest >> testOneSpaceIndent [
	
	self validate: ':i | i to: 10 do: [:j | Transcript show:i; show: j]'
		isFormattedAs: ' :i | 
i to: 10 do: [ :j | 
 Transcript
  show: i;
  show: j ] '
	with: #oneSpaceIndentConfiguration.
]

{ #category : #tests }
EFBlockExpressionTest >> testSpace [

	self validate: '2' isFormattedAs: ' 2 '
]

{ #category : #tests }
EFBlockExpressionTest >> testTabIndent [
	self validate: ':i | i to: 10 do: [:j | Transcript show:i; show: j]'
	isFormattedAs: ' :i | 
i to: 10 do: [ :j | 
	Transcript
		show: i;
		show: j ] '
	 with: #tabIndentConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testTabIndentIsNotAffectedByNumberOfSpace [

	self validate: ':i | i to: 10 do: [:j | Transcript show:i; show: j]'
	 isFormattedAs: ' :i | 
i to: 10 do: [ :j | 
	Transcript
		show: i;
		show: j ] '
	 with: #tabIndentIsnotAffectedConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testTooLongBlockBody [
	self validate: 'tooLongStatement' isFormattedAs:
' 
tooLongStatement ' with: #tooLongStatementConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testTooLongBlockBodyWithArgument [
	self validate: ':i | tooLongStatement' isFormattedAs:
' :i | 
tooLongStatement ' with: #tooLongStatementConfiguration
]

{ #category : #tests }
EFBlockExpressionTest >> testnumberOfSpaceChangedAfterIndentCharacter [

	self validate: ':i | i to: 10 do: [:j | Transcript show:i; show: j]'
	isFormattedAs: ' :i | 
i to: 10 do: [ :j | 
  Transcript
    show: i;
    show: j ] '
	with: #numberOfSpaceChangedAfterIndentCharacterConfiguration
]

{ #category : #configurations }
EFBlockExpressionTest >> tooLongStatementConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideBlock: 1;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentStyle: #tabulation;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentCloseToStatements:true;
		maxLineLength:10;
		indentExtraSpaces: 0
]

{ #category : #helpers }
EFBlockExpressionTest >> validate: stringInsideBlock isFormattedAs: expectedStringInsideBlock [
	self 
		validate: stringInsideBlock 
		isFormattedAs: expectedStringInsideBlock 
		with: #basicConfiguration
]

{ #category : #helpers }
EFBlockExpressionTest >> validate: stringInsideBlock isFormattedAs: expectedStringInsideBlock with: aConfiguration [

	| expr source |
	expr := self parserClass parseExpression: self openBracket , stringInsideBlock, self closeBracket.
	configurationSelector := aConfiguration.
	source := self formatter format: expr.
	self assert: source equals: self openBracket , expectedStringInsideBlock , self closeBracket
]
