Class {
	#name : #EFCascadeExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFCascadeExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		newLineBeforeFirstCascade: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		newLineAfterCascade:false;
		indentStyle: #tabulation;
		indentExtraSpaces:0;
		indentsForKeywords: 1
]

{ #category : #configurations }
EFCascadeExpressionTest >> multilineMessageConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: true;
		formatCommentCloseToStatements:false;
		multiLineMessages:#(#multi:line:);
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:true;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false;
		numberOfSpacesInsideBlock:1;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		indentsForKeywords:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFCascadeExpressionTest >> newLineAfterCascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

"	^ self contextClass new

"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: false;
		formatCommentCloseToStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFCascadeExpressionTest >> newLineBeforeAndAfterCascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: true;
		formatCommentCloseToStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFCascadeExpressionTest >> newLineBeforeCascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"		

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: true;
		formatCommentCloseToStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces:0

]

{ #category : #configurations }
EFCascadeExpressionTest >> noNewLineAfterCascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: false;
		formatCommentCloseToStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFCascadeExpressionTest >> noNewLineBeforeCascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: false;
		formatCommentCloseToStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentStyle: #tabulation;
		newLineBeforeFirstKeyword:false

]

{ #category : #configurations }
EFCascadeExpressionTest >> spaceIndentConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		newLineAfterCascade:true;
		newLineBeforeFirstCascade: true;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		numberOfSpacesInIndent: 1;
		indentStyle: #space;
		indentExtraSpaces:0
]

{ #category : #tests }
EFCascadeExpressionTest >> testCommentAfterMessages [
	| source |
	self flag: #tocheck. "We do we lose the first comment."
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression:
			'Transcript show:1 "firstComment";
	cr "secondComment";
	cr "thirdComment"'.
	self
		assert: source
		equals:
'Transcript show: 1; "secondComment"cr; "thirdComment"
		cr'
]

{ #category : #tests }
EFCascadeExpressionTest >> testCommentBeforeMessage [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript show:1; "cr" cr'.
	self flag: #tocheck. "the comment is glued and this is ugly"
	self
		assert: source
		equals:
			'Transcript show: 1; "cr"cr'
]

{ #category : #tests }
EFCascadeExpressionTest >> testFormatArgument [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 between:2 + 2 and:5*5; +1'.
	self assert: source equals: '1 between: 2 + 2 and: 5 * 5; + 1'
]

{ #category : #tests }
EFCascadeExpressionTest >> testFormatMessage [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: '1 *4; +1'.
	self assert: source equals: '1 * 4; + 1'
]

{ #category : #tests }
EFCascadeExpressionTest >> testFormatMultilineMessage [
	| source |
	configurationSelector := #multilineMessageConfiguration.
	source := self
		formatExpression: '1 between:2 and:5; to:4 do:[:each| each factorial. each + 1]'.
	self
		assert: source
		equals:
'1
	between: 2 and: 5;
	to: 4 do: [ :each | 
		each factorial.
		each + 1 ]'
]

{ #category : #tests }
EFCascadeExpressionTest >> testFormatMultilineMessage2 [
	| source |
	configurationSelector := #multilineMessageConfiguration.
	source := self formatExpression: '1 factorial; multi:1 line:2'.
	self
		assert: source
		equals:
'1
	factorial;
	multi: 1
	line: 2'
]

{ #category : #tests }
EFCascadeExpressionTest >> testFormatReceiver [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: 'a:=1 factorial; +1'.
	self assert: source equals: 'a := 1 factorial; + 1'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNewLineAfterCascade [
	| source |
	configurationSelector := #newLineAfterCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self
		assert: source
		equals:
			'Foo a;
	b;
	c'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNewLineAfterCascade2 [
	| source |
	configurationSelector := #newLineAfterCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals:
			'OrderedCollection new add: #a;
	add: #b;
	yourself'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNewLineBeforeAndAfterCascade [
	| source |
	configurationSelector := #newLineBeforeAndAfterCascadeConfiguration.
	source := self formatExpression: 'Transcript cr;cr;cr'.
	self
		assert: source
		equals:
			'Transcript
	cr;
	cr;
	cr'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNewLineBeforeCascade [
	| source |
	configurationSelector := #newLineBeforeCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self
		assert: source
		equals:
			'Foo
	a; b; c'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNewLineBeforeCascade2 [
	| source |
	configurationSelector := #newLineBeforeCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals:
			'OrderedCollection new
	add: #a; add: #b; yourself'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNoNewLineAfterCascade [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self assert: source equals: 'Foo a; b; c'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNoNewLineAfterCascade2 [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals: 'OrderedCollection new add: #a; add: #b; yourself'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNoNewLineBeforeCascade [
	| source |
	configurationSelector := #noNewLineBeforeCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self assert: source equals: 'Foo a; b; c'
]

{ #category : #tests }
EFCascadeExpressionTest >> testNoNewLineBeforeCascade2 [
	| source |
	configurationSelector := #noNewLineBeforeCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals: 'OrderedCollection new add: #a; add: #b; yourself'
]

{ #category : #tests }
EFCascadeExpressionTest >> testOneSpaceIndent [
	| source |
	configurationSelector := #spaceIndentConfiguration.
	source := self
		formatExpression:
			'Transcript show:1; cr; cr'.
	self
		assert: source
		equals:
'Transcript
 show: 1;
 cr;
 cr'
]

{ #category : #tests }
EFCascadeExpressionTest >> testTooLongArgument [
	| source |
	configurationSelector := #tooLongArgumentConfiguration.
	source := self formatExpression: 'foo a;b: toLongArgument'.
	self
		assert: source
		equals:
'foo a; b:
		toLongArgument'
]

{ #category : #tests }
EFCascadeExpressionTest >> testTooLongArgument2 [
	| source |
	configurationSelector := #tooLongArgumentConfigurationWithNewLineAfterCascade.
	source := self formatExpression: 'foo a;b: toLongArgument'.
	self
		assert: source
		equals:
'foo a;
	b:
		toLongArgument'
]

{ #category : #configurations }
EFCascadeExpressionTest >> tooLongArgumentConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		newLineBeforeFirstCascade: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:10;
		keepBlockInMessage:false;
		newLineAfterCascade:false;
		indentStyle: #tabulation;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFCascadeExpressionTest >> tooLongArgumentConfigurationWithNewLineAfterCascade [
	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		newLineBeforeFirstCascade: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:10;
		keepBlockInMessage:false;
		newLineAfterCascade:true;
		indentStyle: #tabulation;
		indentExtraSpaces:0
]
