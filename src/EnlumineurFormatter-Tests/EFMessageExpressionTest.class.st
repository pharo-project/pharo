Class {
	#name : #EFMessageExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFMessageExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 70;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> blockAndCascadeInMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: false;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 1;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInsideParentheses: 0 ;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFMessageExpressionTest >> blockAndParenthesesInMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInsideParentheses: 1 ;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFMessageExpressionTest >> blockInMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> blockInMessageTooLongConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 20;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> dontKeepBlockInMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: false;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> extraIndentWithThreeSpacesInsideParenthesesConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 0;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: false;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInsideParentheses: 3 ;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFMessageExpressionTest >> formatStatementsWithCommentsConfiguration [

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:true;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #configurations }
EFMessageExpressionTest >> ifTrueIfFalseMutlilineMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages:#(#ifTrue:ifFalse:);
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0;
		newLineBeforeFirstCascade: true;
		numberOfSpacesInsideParentheses: 1 ;
		newLineAfterCascade: true
]

{ #category : #configurations }
EFMessageExpressionTest >> keepBlockInMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> littleMaxLineLengthConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 20;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		numberOfSpacesInsideBlock: 1;
		numberOfSpacesAfterCaretSymbolInReturn: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> multiLineMessageConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: #(#multi:line:);
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 1;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> newLineBeforeFirstKeywordConfiguration [

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:true;
		keepBlockInMessage:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> noIdentsForKeyWordConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: false;
		numberOfArgumentsForMultiLine: 1;
		indentsForKeywords: 0;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> noTraditionalBinaryPrecedenceConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: false;
		keepBlockInMessage: false;
		numberOfSpacesInsideParentheses: 0;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> oneArgumentPerLineConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 1;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: false;
		indentExtraSpaces: 0;
		numberOfSpacesInsideParentheses: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> oneIdentsForKeyWordConfiguration [

	^ self contextClass new
		newLineBeforeFirstKeyword: true;
		numberOfArgumentsForMultiLine: 1;
		indentsForKeywords: 1
]

{ #category : #configurations }
EFMessageExpressionTest >> oneLineMessageConfiguration [

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:#(#one:line:message:);
		numberOfArgumentsForMultiLine:2;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> parentheseConfiguration [
	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 0;
		maxLineLength: 70;
		newLineBeforeFirstKeyword: false;
		keepBlockInMessage: false;
		indentExtraSpaces: 0;
		numberOfSpacesInsideParentheses: 0
]

{ #category : #configurations }
EFMessageExpressionTest >> shortMaxLineLengthConfiguration [

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 1;
		maxLineLength: 13;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> simpleConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: false;
		keepBlockInMessage: false;
		numberOfSpacesInsideParentheses: 0;
		indentExtraSpaces: 0
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryFormatParameter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 + (3     factorial)'.
	self assert: source equals: '1 + 3 factorial'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=''abc'' + 2'.
	self assert: source equals: 'a := ''abc'' + 2'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryMessageComments [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 + 1 "aComment"'.
	self assert: source equals: '1 + 1 "aComment"'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryMessageFormatCommentsWithStatement [
	| source |
	self flag: #toCheck. "Why would we lose the comment?"
	configurationSelector := #formatStatementsWithCommentsConfiguration.
	source := self formatExpression: '1 + 1 "aComment"'.
	self assert: source equals: '1 + 1'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1+2          '.
	self assert: source equals: '1 + 2'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   1+2'.
	self assert: source equals: '1 + 2'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1     +     2'.
	self assert: source equals: '1 + 2'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryOneArgumentPerLine [
	| source |
	configurationSelector := #oneArgumentPerLineConfiguration.
	source := self formatExpression: '1+2'.
	self
		assert: source
		equals:
'1
+ 2'
]

{ #category : #'test binary' }
EFMessageExpressionTest >> testBinaryOneArgumentPerLine2 [
	| source |
	configurationSelector := #oneArgumentPerLineConfiguration.
	source := self formatExpression: '(1+2)*3'.
	self
		assert: source
		equals:
			'1
+ 2
* 3'
]

{ #category : #'test block arg' }
EFMessageExpressionTest >> testBlockParameterOnTheMethodLineWhenMultiline [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression: '1 to: 10 do:[ :i | Transcript show:i. Transcript cr. Transcript cr]'.
	self
		assert: source
		equals:
'1 to: 10 do: [ :i | 
	Transcript show: i.
	Transcript cr.
	Transcript cr ]'
]

{ #category : #'test block arg' }
EFMessageExpressionTest >> testBlockParameterWhenTheLineWillBeTooLong [
	| source |
	configurationSelector := #blockInMessageTooLongConfiguration.
	source := self formatExpression: 'aMessage take:[ :i | i factorial]'.
	self
		assert: source
		equals: 
'aMessage take: [ :i | 
	i factorial ]'
]

{ #category : #'test block arg' }
EFMessageExpressionTest >> testBlockParameterWhenTheLineWillBeTooLong2 [
	| source |
	configurationSelector := #blockInMessageTooLongConfiguration.
	source := self formatExpression: 'aMessage take:[ :iiiiiiiiiiiiiii | i factorial]'.
	self
		assert: source
		equals:
'aMessage take: [ 
	:iiiiiiiiiiiiiii | 
	i factorial ]'
]

{ #category : #'test block arg' }
EFMessageExpressionTest >> testBlockParameterWhenTheLineWillNotBeTooLong [
	| source |
	configurationSelector := #blockInMessageConfiguration.
	source := self formatExpression: 'aMessage take:[ :i | i]'.
	self assert: source equals: 'aMessage take: [ :i | i ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testDontKeepBlockInMessage [
	| source |
	configurationSelector := #dontKeepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ :i | i := i factorial. Transcript show:i ]'.
	self
		assert: source
		equals:
'aBoolean ifTrue:
	[ :i | 
	i := i factorial.
	Transcript show: i ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testDontKeepBlockInMessage2 [
	| source |
	configurationSelector := #dontKeepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ :i | i ]'.
	self
		assert: source
		equals:
'aBoolean ifTrue: [ :i | i ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testDontKeepBlockInMessage3 [
	| source |
	configurationSelector := #dontKeepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ Transcript cr. Transcript cr ]'.
	self
		assert: source
		equals:
'aBoolean ifTrue:
	[ Transcript cr.
	Transcript cr ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testExtraIndentInParentheses [
	| source |
	configurationSelector := #blockAndParenthesesInMessageConfiguration.
	source := self formatExpression: '( [ :i | 	i factorial; +3 ] value: 15 )*3'.
	self assert: source equals:
'( [ :i | 
  i
	  factorial;
	  + 3 ] value: 15 ) * 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testExtraIndentInParenthesesWhenThreeSpacesInsideParentheses [
	| source |
	configurationSelector := #extraIndentWithThreeSpacesInsideParenthesesConfiguration.
	source := self formatExpression:'( [ :i | 	i factorial; +3 ] value: 15 )*3'.
	self assert: source equals:
'(   [ :i | 
    i
	    factorial;
	    + 3 ] value: 15   ) * 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testIndentsForKeywords1 [
	| source |
	configurationSelector := #oneIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
	k1: 1
	k2: 2
	k3: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testIndentsForKeywords2 [
	| source |
	configurationSelector := #twoIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
		k1: 1
		k2: 2
		k3: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testKeepBlockInMessage [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ :i | i := i factorial. Transcript show:i ]'.
	self
		assert: source
		equals:
'aBoolean ifTrue: [ :i | 
	i := i factorial.
	Transcript show: i ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testKeepBlockInMessageMutlilineNewLine [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression: 'aBoolean  ifTrue:[ Transcript cr. Transcript cr. Transcript cr]'.
	self
		assert: source
		equals:
'aBoolean ifTrue: [ 
	Transcript cr.
	Transcript cr.
	Transcript cr ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testKeepBlockInMessageMutlilineSpace [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ 12 factorial. 12 factorial. 3
	factorial]'.
	self
		assert: source
		equals:
'aBoolean ifTrue: [ 
	12 factorial.
	12 factorial.
	3 factorial ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testKeepBlockInMessageNotMultilineSpace [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[^1]'.
	self
		assert: source
		equals:
			'aBoolean ifTrue: [ ^1 ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testKeepBlockInMessageNotMutlilineNewLine [
	| source |
	configurationSelector := #littleMaxLineLengthConfiguration.
	source := self
		formatExpression: 'aBoolean ifTrue:[LongLongLongExpression]'.
	self
		assert: source
		equals:
			'aBoolean ifTrue: [ 
	LongLongLongExpression ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordComments [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 between: 2"firstComment" and: 3 "secondComment"'.
	self assert: source equals: '1 between: 2 "firstComment" and: 3 "secondComment"'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordCommentsFormatCommentsWithStatement [
	| source |
	self flag: #toCheck. "why no more comment." 
	configurationSelector := #formatStatementsWithCommentsConfiguration.
	source := self formatExpression: '1 between: 2"firstComment" and: 3 "secondComment"'.
	self assert: source equals: '1 between: 2 and: 3'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordFormatParameter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: '3 between:2      factorial and: 4+5'.
	self assert: source equals: '3 between: 2 factorial and: 4 + 5'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2 between: 4-2 and: 3     factorial'.
	self assert: source equals: '2 between: 4 - 2 and: 3 factorial'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordFormatReceiver2 [
	| source |
	configurationSelector := #parentheseConfiguration.
	source := self formatExpression: '(2 between: 4-2 and: 3)     factorial'.
	self assert: source equals: '(2 between: 4 - 2 and: 3) factorial'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordFormatReceiver3 [
	| source |
	configurationSelector := #parentheseConfiguration.
	source := self formatExpression: '(d at: 2)factorial'.
	self assert: source equals: '(d at: 2) factorial'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordFormatReceiverAssignment [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=''abc'' at: 2'.
	self assert: source equals: 'a := ''abc'' at: 2'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2 between: 1 and: 3          '.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   2 between: 1 and: 3'.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2      between:     1   and:    3'.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: '1 to: 100 by: 2 do: [ :i | transcript show: i;cr; cr]'.
	self assert: source equals:
'1 to: 100 by: 2 do: [ :i | 
	transcript
		show: i;
		cr;
		cr ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine10 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5.'.
	self assert: source equals:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine11 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo bar3: 5 bar2: [ 
	1 + 1.
	2 + 2 ]'.
	self assert: source equals:
'foo bar3: 5 bar2: [ 
	1 + 1.
	2 + 2 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine12 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5'.
	self assert: source equals:
'foo
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine13 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo bar3: 5 bar3: 5 bar2: [ 
	1 + 1.
	2 + 2 ]'.
	self assert: source equals:
'foo bar3: 5 bar3: 5 bar2: [ 
	1 + 1.
	2 + 2 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine14 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5 
	bar3: 5'.
	self assert: source equals:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5
	bar3: 5'.
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine15 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'self
		indent:
			(self do:[1. 2])
		around: [ 1. 2 ]'.
	self assert: source equals:
'self
	indent: (self do: [ 
			 1.
			 2 ])
	around: [ 
		1.
		2 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine2 [
	| source |
	configurationSelector := #ifTrueIfFalseMutlilineMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[ ^ 1] ifFalse:[ tmp := tmp + a. ^tmp]'.
	self assert: source equals:
'aBoolean
	ifTrue: [ ^1 ]
	ifFalse: [ 
		tmp := tmp + a.
		^tmp ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine3 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[ tmp := tmp + a. ^ tmp] ifFalse:[ ^ 0 ]'.
	self assert: source equals:
'aBoolean
	ifTrue: [ 
		tmp := tmp + a.
		^ tmp ]
	ifFalse: [ ^ 0 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine4 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[ tmp := tmp + a. ^ tmp] ifFalse:[ tmp := tmp + a. ^tmp]'.
	self assert: source equals:
'aBoolean
	ifTrue: [ 
		tmp := tmp + a.
		^ tmp ]
	ifFalse: [ 
		tmp := tmp + a.
		^ tmp ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine5 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[ ^ 1] ifFalse:[ tmp := tmp + a. ^tmp]'.
	self assert: source equals:
'aBoolean ifTrue: [ ^ 1 ] ifFalse: [ 
	tmp := tmp + a.
	^ tmp ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine6 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo 
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]
	bar3: 5'.
	self assert: source equals:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]
	bar3: 5'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine7 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo 
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]'.
	self assert: source equals:
'foo
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine8 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5
	bar2: [ 
		1 + 1.
		2 + 2 ]'.
	self assert: source equals:
'foo
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar3: 5
	bar2: [ 
		1 + 1.
		2 + 2 ]'.
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnMultipleLine9 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression:
'foo
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]
	bar3: 5'.
	self assert: source equals:
'foo
	bar3: 5
	bar: [ 
		1 + 2.
		2 + 2 ]
	bar2: [ 
		1 + 1.
		2 + 2 ]
	bar3: 5'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnTheSameLine [
	| source |
	configurationSelector := #blockInMessageConfiguration.
	source := self formatExpression: '1 to: 100 do: [ :i | Transcript show: i ]'.
	self assert: source equals: '1 to: 100 do: [ :i | Transcript show: i ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnTheSameLine2 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: '1 to: 100 by: 2 do: [ :i | Transcript show: i]'.
	self assert: source equals: '1 to: 100 by: 2 do: [ :i | Transcript show: i ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOnTheSameLine3 [
	| source |
	configurationSelector := #blockAndCascadeInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[ ^ 1] ifFalse:[ ^ 0 ]'.
	self assert: source equals: 'aBoolean ifTrue: [ ^ 1 ] ifFalse: [ ^ 0 ]'
]

{ #category : #'tests keyword' }
EFMessageExpressionTest >> testKeywordOneArgumentPerLine [
	| source |
	configurationSelector := #oneArgumentPerLineConfiguration.
	source := self formatExpression: '2      between:     1   and:    3'.
	self
		assert: source
		equals:
'2
between: 1
and: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testMultiLineMessage [
	| source |
	configurationSelector := #multiLineMessageConfiguration.
	source := self formatExpression: 'foo multi:1 line:2 '.
	self
		assert: source
		equals:
'foo
	multi: 1
	line: 2'
]

{ #category : #tests }
EFMessageExpressionTest >> testNewLineBeforeFirstKeyword [
	| source |
	configurationSelector := #newLineBeforeFirstKeywordConfiguration.
	source := self formatExpression: '2 between: 1 and: 3'.
	self
		assert: source
		equals:
			'2 between: 1 and: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testNewLineBeforeFirstKeywordDoesntAffectBinary [
	| source |
	configurationSelector := #newLineBeforeFirstKeywordConfiguration.
	source := self formatExpression: '1+1'.
	self
		assert: source
		equals:'1 + 1'
]

{ #category : #tests }
EFMessageExpressionTest >> testNewLineBeforeStatementsWhenMultilineBlock [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'x = 0 ifFalse: [ tan := y asFloat / x asFloat.
	theta := tan arcCos ]'.
	self
		assert: source
		equals:
'x = 0 ifFalse: [ 
	tan := y asFloat / x asFloat.
	theta := tan arcCos ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testNewLineWhenSelectorAndBeginingOfParametersWillBeTooLong [
	| source |
	configurationSelector := #blockAndParenthesesInMessageConfiguration.
	source := self formatExpression:
'(newLineBetweenArguments and: [ aMessageNode isKeyword ]) ifTrue: [ self
	indentsForKeywords ]
	ifFalse: [ 0 ]'.
	self assert: source equals:
'( newLineBetweenArguments and: [ aMessageNode isKeyword ] )
	ifTrue: [ self indentsForKeywords ]
	ifFalse: [ 0 ]'.
]

{ #category : #tests }
EFMessageExpressionTest >> testNoIndentsForKeywords [
	| source |
	configurationSelector := #noIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
k1: 1
k2: 2
k3: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testNoNewLineBetweenVariableAndIfTrue [
	| source |
	configurationSelector := #blockInMessageConfiguration.
	source := self formatExpression: 'x
		ifTrue: [ 
			  aBitBlt copyBitsColor: foreColorVal
				  alpha: foreColorAlpha
				  gammaTable: gammaTable
				  ungammaTable: gammaInverseTable ]'.
	self assert: source equals: 
'x ifTrue: [ 
	aBitBlt
		copyBitsColor: foreColorVal
		alpha: foreColorAlpha
		gammaTable: gammaTable
		ungammaTable: gammaInverseTable ]'
]

{ #category : #tests }
EFMessageExpressionTest >> testNoNewLineForBeginingOfATooLongArgument [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: 'self parseMethod: 	''a
b'''.
	self assert: source equals: 'self parseMethod: ''a
b'''
]

{ #category : #tests }
EFMessageExpressionTest >> testNoNewLineForBeginingOfATooLongArgument2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: 'self
		parseMethod:
			''foo: abd bar: cde  
^ abd + cde'''.
	self assert: source equals:
'self parseMethod: ''foo: abd bar: cde  
^ abd + cde'''
]

{ #category : #tests }
EFMessageExpressionTest >> testNoTraditionalBinaryPrecedence [
	| source |
	configurationSelector := #noTraditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '2+3*4'.
	self assert: source equals: '2 + 3 * 4'
]

{ #category : #tests }
EFMessageExpressionTest >> testNoTraditionalBinaryPrecedence2 [
	| source |
	configurationSelector := #noTraditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '(2 + 3) * 4'.
	self assert: source equals: '2 + 3 * 4'
]

{ #category : #tests }
EFMessageExpressionTest >> testNoTraditionalBinaryPrecedence3 [
	| source |
	configurationSelector := #noTraditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '2+3*4+5 * 2'.
	self assert: source equals: '2 + 3 * 4 + 5 * 2'
]

{ #category : #tests }
EFMessageExpressionTest >> testOneLineMessage [
	| source |
	configurationSelector := #oneLineMessageConfiguration.
	source := self formatExpression: 'foo one:1 line:2 message:3'.
	self assert: source equals: 'foo one: 1 line: 2 message: 3'
]

{ #category : #tests }
EFMessageExpressionTest >> testSpacesInsideParentheses [
	| source |
	configurationSelector := #twoSpacesInsideParenthesesConfiguration.
	source := self formatExpression: '1+(2*3)'.
	self assert: source equals: '1 + (  2 * 3  )'
]

{ #category : #tests }
EFMessageExpressionTest >> testTooLongArgument [
	| source |
	configurationSelector := #shortMaxLineLengthConfiguration.
	source := self formatExpression: 'foo message: TooLongArgument'.
	self
		assert: source
		equals:
'foo message:
	TooLongArgument'
]

{ #category : #tests }
EFMessageExpressionTest >> testTooLongLineWithArgument [
	| source |
	configurationSelector := #shortMaxLineLengthConfiguration.
	source := self formatExpression: 'foo message: argument'.
	self
		assert: source
		equals:
'foo message:
	argument'
]

{ #category : #tests }
EFMessageExpressionTest >> testTraditionalBinaryPrecedence [
	| source |
	configurationSelector := #simpleConfiguration.
	source := self formatExpression: '2+3*4'.
	self assert: source equals: '2 + 3 * 4'
]

{ #category : #tests }
EFMessageExpressionTest >> testTraditionalBinaryPrecedence2 [
	| source |
	configurationSelector := #simpleConfiguration.
	source := self formatExpression: '2*3+4'.
	self assert: source equals: '2 * 3 + 4'
]

{ #category : #tests }
EFMessageExpressionTest >> testTraditionalBinaryPrecedence3 [
	| source |
	configurationSelector := #simpleConfiguration.
	source := self formatExpression: '2+3*4+5 * 2'.
	self assert: source equals: '2 + 3 * 4 + 5 * 2'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=1 factorial'.
	self assert: source equals: 'a := 1 factorial'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryFormatReceiver2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=1 factorial foo bar'.
	self assert: source equals: 'a := 1 factorial foo bar'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryMessageComments [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 factorial "aComment"'.
	self assert: source equals: '1 factorial "aComment"'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryMessageFormatCommentsWithStatement [
	| source |
	self flag: #toCheck. "why do we lose this comment"
	configurationSelector := #formatStatementsWithCommentsConfiguration.
	source := self formatExpression: '1 factorial "aComment"'.
	self assert: source equals: '1 factorial'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript cr        '.
	self assert: source equals: 'Transcript cr'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryNoExtraSpacesAfter2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript cr cr cr       '.
	self assert: source equals: 'Transcript cr cr cr'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   Transcript cr'.
	self assert: source equals: 'Transcript cr'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript        cr'.
	self assert: source equals: 'Transcript cr'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryNoExtraSpacesBetween2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript        cr cr       cr     cr'.
	self assert: source equals: 'Transcript cr cr cr cr'
]

{ #category : #'test unary' }
EFMessageExpressionTest >> testUnaryTooLongArgument [
	| source |
	configurationSelector := #shortMaxLineLengthConfiguration.
	source := self formatExpression: 'foo messageTooLongUnaryyyy'.
	self
		assert: source
		equals:
'foo
	messageTooLongUnaryyyy'
]

{ #category : #configurations }
EFMessageExpressionTest >> twoIdentsForKeyWordConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 1;
		indentsForKeywords: 2;
		maxLineLength: 50;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMessageExpressionTest >> twoSpacesInsideParenthesesConfiguration [

	^ self contextClass basicNew
		numberOfSpacesInsideParentheses: 2;
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false;
		indentExtraSpaces: 0
]
