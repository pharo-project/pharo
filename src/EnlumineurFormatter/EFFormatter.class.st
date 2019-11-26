"
I'm a new pretty printer for Pharo.
I started as a copy of the BlueInk pretty printer. To avoid to break everything we clone BIPrettyPrinter and started to evolve 
and made sure that all the tests are in sync with the implementation. 

Note that we are nice guys since we implemented 180 tests for the old pretty printer.
Now such tests can not be systematically applied on Enlumineur this is why we cloned them too. 

Pay attention my name should finish with Formatter so that the Settings proposes me to the user along with the other formatters.


"
Class {
	#name : #EFFormatter,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'codeStream',
		'indent',
		'lookaheadCode',
		'originalSource',
		'lineStart',
		'context',
		'isInCascadeNode',
		'isInMethodNode',
		'temporaryLookAheadCode'
	],
	#classVars : [
		'DefaultPrettyPrintContext',
		'FormatAsYouReadPolicy'
	],
	#category : #'EnlumineurFormatter-Core'
}

{ #category : #private }
EFFormatter class >> announceASettingChange [
	SystemAnnouncer uniqueInstance announce: EFSettingChanged 
]

{ #category : #private }
EFFormatter class >> defaultPrettyPrintContext [
	^ DefaultPrettyPrintContext
]

{ #category : #accessing }
EFFormatter class >> defaultPrettyPrintContext: aContext [ 
	DefaultPrettyPrintContext := aContext 
]

{ #category : #public }
EFFormatter class >> format: aParseTree [ 
	^self format: aParseTree withIndents: 0
]

{ #category : #public }
EFFormatter class >> format: aParseTree withIndents: anInteger [ 
	^ self new
		indent: anInteger;
		format: aParseTree
]

{ #category : #accessing }
EFFormatter class >> formatAsYouReadPolicy [
	^ FormatAsYouReadPolicy
]

{ #category : #accessing }
EFFormatter class >> formatAsYouReadPolicy: anObject [
	FormatAsYouReadPolicy := anObject
]

{ #category : #accessing }
EFFormatter class >> formatCommentCloseToStatements [
	^ DefaultPrettyPrintContext formatCommentCloseToStatements
]

{ #category : #accessing }
EFFormatter class >> formatCommentCloseToStatements: aBoolean [
	DefaultPrettyPrintContext formatCommentCloseToStatements: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> indentString [
	^ DefaultPrettyPrintContext indentString
]

{ #category : #accessing }
EFFormatter class >> indentStyle [
	^ DefaultPrettyPrintContext indentStyle
]

{ #category : #accessing }
EFFormatter class >> indentStyle: aSymbole [
	DefaultPrettyPrintContext indentStyle: aSymbole.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> indentsForKeywords [
	^ DefaultPrettyPrintContext indentsForKeywords
]

{ #category : #accessing }
EFFormatter class >> indentsForKeywords: anInteger [
	DefaultPrettyPrintContext indentsForKeywords: anInteger.
	self announceASettingChange
]

{ #category : #initialization }
EFFormatter class >> initialize [
	"self initialize"

	FormatAsYouReadPolicy := false.
	DefaultPrettyPrintContext := EFContext new
]

{ #category : #private }
EFFormatter class >> isPrettyPrinter [
	^ true
]

{ #category : #accessing }
EFFormatter class >> keepBlockInMessage [
	"Return a boolean for 
		1 to: 4 do: [ :i | 
			
	vs. 
	
		1 to: 4 do: 
			[ :i |"
		
	^ DefaultPrettyPrintContext keepBlockInMessage
]

{ #category : #accessing }
EFFormatter class >> keepBlockInMessage: aBoolean [
	DefaultPrettyPrintContext keepBlockInMessage: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> lineUpBlockBrackets [
	^ DefaultPrettyPrintContext lineUpBlockBrackets
]

{ #category : #accessing }
EFFormatter class >> lineUpBlockBrackets: aBoolean [
	DefaultPrettyPrintContext lineUpBlockBrackets: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> maxLineLength [
	^ DefaultPrettyPrintContext maxLineLength
]

{ #category : #accessing }
EFFormatter class >> maxLineLength: anInteger [
	DefaultPrettyPrintContext maxLineLength: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> methodSignatureOnMultipleLines [
	^ DefaultPrettyPrintContext methodSignatureOnMultipleLines
]

{ #category : #accessing }
EFFormatter class >> methodSignatureOnMultipleLines: aBoolean [
	DefaultPrettyPrintContext methodSignatureOnMultipleLines: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> minimumNewLinesBetweenStatements [
	^ DefaultPrettyPrintContext minimumNewLinesBetweenStatements
]

{ #category : #accessing }
EFFormatter class >> minimumNewLinesBetweenStatements: anInteger [
	DefaultPrettyPrintContext minimumNewLinesBetweenStatements: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> multiLineMessages [
	^ DefaultPrettyPrintContext multiLineMessages printString
]

{ #category : #accessing }
EFFormatter class >> multiLineMessages: aString [
	DefaultPrettyPrintContext multiLineMessages: (self compiler evaluate: aString).
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> newLineAfterCascade [
	^ DefaultPrettyPrintContext newLineAfterCascade
]

{ #category : #accessing }
EFFormatter class >> newLineAfterCascade: aBoolean [
	DefaultPrettyPrintContext newLineAfterCascade: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> newLineBeforeFirstCascade [
	^ DefaultPrettyPrintContext newLineBeforeFirstCascade
]

{ #category : #accessing }
EFFormatter class >> newLineBeforeFirstCascade: aBoolean [
	DefaultPrettyPrintContext newLineBeforeFirstCascade: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> newLineBeforeFirstKeyword [
	^ DefaultPrettyPrintContext newLineBeforeFirstKeyword
]

{ #category : #accessing }
EFFormatter class >> newLineBeforeFirstKeyword: aBoolean [
	DefaultPrettyPrintContext newLineBeforeFirstKeyword: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfArgumentsForMultiLine [
	^ DefaultPrettyPrintContext numberOfArgumentsForMultiLine
]

{ #category : #accessing }
EFFormatter class >> numberOfArgumentsForMultiLine: anInteger [
	DefaultPrettyPrintContext numberOfArgumentsForMultiLine: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterMethodComment [
	^ DefaultPrettyPrintContext numberOfNewLinesAfterMethodComment
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterMethodComment: anInteger [
	DefaultPrettyPrintContext numberOfNewLinesAfterMethodComment: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterMethodSignature [
	^ DefaultPrettyPrintContext numberOfNewLinesAfterMethodSignature
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterMethodSignature: anInteger [
	DefaultPrettyPrintContext numberOfNewLinesAfterMethodSignature: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterTemporaries [
	^ DefaultPrettyPrintContext numberOfNewLinesAfterTemporaries
]

{ #category : #accessing }
EFFormatter class >> numberOfNewLinesAfterTemporaries: anInteger [
	DefaultPrettyPrintContext numberOfNewLinesAfterTemporaries: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesAfterCaretSymbolInReturn [
	^ DefaultPrettyPrintContext numberOfSpacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesAfterCaretSymbolInReturn: anInteger [
	DefaultPrettyPrintContext numberOfSpacesAfterCaretSymbolInReturn: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesBeforeDotInDynamicArray [
	^ DefaultPrettyPrintContext numberOfSpacesBeforeDotInDynamicArray
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInIndent [
	^ DefaultPrettyPrintContext numberOfSpacesInIndent
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInIndent: anInteger [
	DefaultPrettyPrintContext numberOfSpacesInIndent: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideArray [
	^ DefaultPrettyPrintContext numberOfSpacesInsideArray
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideArray: anInteger [
	DefaultPrettyPrintContext numberOfSpacesInsideArray: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideBlock [
	^ DefaultPrettyPrintContext numberOfSpacesInsideBlock
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideBlock: anInteger [
	DefaultPrettyPrintContext numberOfSpacesInsideBlock: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideParentheses [
	^ DefaultPrettyPrintContext numberOfSpacesInsideParentheses
]

{ #category : #accessing }
EFFormatter class >> numberOfSpacesInsideParentheses: anInteger [
	DefaultPrettyPrintContext numberOfSpacesInsideParentheses: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> oneLineMessages [
	^ DefaultPrettyPrintContext oneLineMessages printString
]

{ #category : #accessing }
EFFormatter class >> oneLineMessages: aString [
	DefaultPrettyPrintContext oneLineMessages: (self compiler evaluate: aString).
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> periodAtEndOfBlock: aBoolean [
	DefaultPrettyPrintContext periodAtEndOfBlock: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> periodAtEndOfMethod: aBoolean [
	DefaultPrettyPrintContext periodAtEndOfMethod: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> periodsAtEndOfBlock [
	^ DefaultPrettyPrintContext periodsAtEndOfBlock
]

{ #category : #accessing }
EFFormatter class >> periodsAtEndOfMethod [
	^ DefaultPrettyPrintContext periodsAtEndOfMethod
]

{ #category : #accessing }
EFFormatter class >> retainBlankLinesBeforeComments [
	^ DefaultPrettyPrintContext retainBlankLinesBeforeComments
]

{ #category : #accessing }
EFFormatter class >> retainBlankLinesBeforeComments: aBoolean [
	DefaultPrettyPrintContext retainBlankLinesBeforeComments: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> retainBlankLinesBetweenStatements [
	^ DefaultPrettyPrintContext retainBlankLinesBetweenStatements
]

{ #category : #accessing }
EFFormatter class >> retainBlankLinesBetweenStatements: aBoolean [
	DefaultPrettyPrintContext retainBlankLinesBetweenStatements: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> selectorAndArgumentCombinedMaxSize [
	^ DefaultPrettyPrintContext selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
EFFormatter class >> selectorAndArgumentCombinedMaxSize: anInteger [
	DefaultPrettyPrintContext selectorAndArgumentCombinedMaxSize: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
EFFormatter class >> settingGroup [
	^ #blueInkEnlumineur
]

{ #category : #settings }
EFFormatter class >> settingsAlignBlockBrackets: aBuilder [

	(aBuilder setting: #lineUpBlockBrackets)
		label: 'Align block brackets';
		description: 'Place a new line before closing a multiline block.
This gives a C-like syntax in Pharo .

When lineUpBlockBrackets is set to true

1 to: 4 do: [ Transcript
		show: i;
		cr
	]

When lineUpBlockBrackets is set to false
1 to: 4 do: [ Transcript
		show: i;
		cr ]
'
]

{ #category : #settings }
EFFormatter class >> settingsCommentCloseToStatement: aBuilder [
	
	(aBuilder setting: #formatCommentCloseToStatements)
		label: 'Place comment close to expression';
		description: 'This setting places a comment close to an expression before the separator. 
	
When formatCommentCloseToStatements is set to true
 1+1."We do an addition" 2+2

When formatCommentCloseToStatements is set to false
 1+1"We do an addition". 2+2'
]

{ #category : #settings }
EFFormatter class >> settingsIndentStyle: aBuilder [

	(aBuilder pickOne: #indentStyle)
		label: 'Indent Style';
		domainValues:#(#tabulation #space);
		description: 'Set the character used for an indentation, it can be space or tabulation. When a space is used you can decide how many spaces are used using numberOfSpacesInIndent.		
Exemple:
aBoolean
	ifTrue:[^1]
	ifFalse:[^0] is the result for tabulation.'
]

{ #category : #settings }
EFFormatter class >> settingsIndentsForKeywords: aBuilder [ 
	
	(aBuilder setting: #indentsForKeywords)
		label: 'Indents for keywords';
		description: 'Number of indentations before each keyword of a multiline message. 
For example:
aBoolean:
		ifTrue:[^1]
		ifFalse:[^0]
is the result for 2 indents'
]

{ #category : #settings }
EFFormatter class >> settingsKeepBlockInMessage: aBuilder [
	
	(aBuilder setting: #keepBlockInMessage)
		label: 'Keep block in message';
		description: 'Keep the begining of the block on the line of the message

When keepBlockInMessage is true: 
1 to: 4 do: [ :i | 
		Transcript
			show: i;cr]
		

When keepBlockInMessage is false: 		
1 to: 4 do:
	[ :i | 
	Transcript
		show: i;
		cr ]
'
]

{ #category : #settings }
EFFormatter class >> settingsMaxLineLength: aBuilder [

	(aBuilder setting: #maxLineLength)
		label: 'Max line length';
		description: 'Set the maximum possible length of a line.
For example:
1 + 2 + 3 + 4
	+ 5 + 6
is the result for 14'
]

{ #category : #settings }
EFFormatter class >> settingsMethodSignatureOnMultipleLines: aBuilder [

	(aBuilder setting: #methodSignatureOnMultipleLines)
		label: 'Method signature on multiple lines';
		description: 'Write a multiline method signature on multiple lines only if set to true.
Pay attention the fact that a method signature is considered multilined depends of the setting selectorAndArgumentCombinedMaxSize).

For example: the method myMethod:foo: will be shown as 

myMethod: arg1
	foo: arg2

is the result of having methodSignatureOnMultipleLines set to true and selectorAndArgumentCombinedMaxSize set to 6'
]

{ #category : #settings }
EFFormatter class >> settingsMinimumNewLinesBetweenStatements: aBuilder [

	(aBuilder setting: #minimumNewLinesBetweenStatements)
		label: 'Minimum new lines between statements';
		description: 'Set the number of new lines between each statement. Its value is usually 0 or 1.

The following is obtained by setting minimumNewLinesBetweenStatements to 2.
	1+1.
	
	2+2.
	
	3+3
'
]

{ #category : #settings }
EFFormatter class >> settingsMultiLineMessages: aBuilder [

	(aBuilder setting: #multiLineMessages)
		label: 'Multi line messages';
		description: 'An array containing all the messages that should be written on multiple lines.
		
Example:
aBoolean
	ifTrue: [ 1 ]
	ifFalse: [ 0 ]
is the result if #ifTrue:ifFalse is in the array'
]

{ #category : #settings }
EFFormatter class >> settingsNewLineAfterCascade: aBuilder [

	(aBuilder setting: #newLineAfterCascade)
		label: 'New line after cascade';
		description: 'To add to a new line after each message in a cascade.

When newLineAfterCascade is set to false.

OrderedCollection new
	add: 1; add: 2; add: 3
	
When newLineAfterCascade is set to true.

OrderedCollection new
	add: 1;
	add: 2;
	add: 3
'
]

{ #category : #settings }
EFFormatter class >> settingsNewLineBeforeFirstCascade: aBuilder [

	(aBuilder setting: #newLineBeforeFirstCascade)
		label: 'New line before cascade';
		description: 'To add a new line before the messages participating in a cascade.

When newLineBeforeFirstCascade set to false

OrderedCollection new add: 1;
	add: 2;
	add: 3

When newLineBeforeFirstCascade set to true

OrderedCollection new
	add: 1;
	add: 2;
	add: 3
'
]

{ #category : #settings }
EFFormatter class >> settingsNewLineBeforeFirstKeyword: aBuilder [

	(aBuilder setting: #newLineBeforeFirstKeyword)
		label: 'New line before first keyword';
		description: 'To add a new line before the first keyword parameter of a message. This can be useful combined with other settings.

When newLineBeforeFirstKeyword is set to true

1
between: 0 and: 4


When newLineBeforeFirstKeyword is set to false

1 between: 0 and: 4

'
]

{ #category : #settings }
EFFormatter class >> settingsNewLinesAfterMethodComment: aBuilder [

	(aBuilder setting: #numberOfNewLinesAfterMethodComment)
		label: 'New lines after method comment';
		description: 'Set the number of new lines after the comment of a method.
		
When numberOfNewLinesAfterMethodComment is set to 2

myMethod
	"myComment"

	^ true
'
]

{ #category : #settings }
EFFormatter class >> settingsNewLinesAfterMethodSignature: aBuilder [

	(aBuilder setting: #numberOfNewLinesAfterMethodSignature)
		label: 'New lines after method signature';
		description: 'To set the number of new lines directly after the method signature.
When numberOfNewLinesAfterMethodSignature is set to 2 new lines

myMethod: arg

	^ true
'
]

{ #category : #settings }
EFFormatter class >> settingsNewLinesAfterTemporaries: aBuilder [

	(aBuilder setting: #numberOfNewLinesAfterTemporaries)
		label: 'New lines after temporaries';
		description: 'To set the number of new lines after the temporaries declaration.

For example setting numberOfNewLinesAfterTemporaries to 2 produces
myMethod
	| a |

	a := 1
'
]

{ #category : #settings }
EFFormatter class >> settingsNumberOfArgumentsForMultiLine: aBuilder [ 
	
	(aBuilder setting: #numberOfArgumentsForMultiLine)
		label: 'Number of arguments for multiLine';
		description: 'To set the number of argument that a message must have to be considered as a multiline message. 

For example when set to 2 we obtain

1
	between: 0
	and: 5
'
]

{ #category : #settings }
EFFormatter class >> settingsNumberOfSpacesInIndent:aBuilder [

	(aBuilder setting: #numberOfSpacesInIndent)
		label: 'Number of spaces in indent';
		description: 'Set the number of spaces representing an indentation if indentStyle is set to space.

The following is the result for 5.

aBoolean
     ifTrue:[ 1 ]
     ifFalse:[ 0 ]
'
]

{ #category : #settings }
EFFormatter class >> settingsOn: aBuilder [
	<systemsettings>
	
	(aBuilder group: self settingGroup)
		target: self;
		parent: #codeFormatters;
		label: 'Enlumineur Pretty Printer';
		description: 'Settings related to automatic code formatting';
		with: [ self settingsMaxLineLength: aBuilder.
			self settingsCommentCloseToStatement: aBuilder.
			self settingsIndentStyle: aBuilder.
			self settingsNumberOfSpacesInIndent: aBuilder.
			self settingsIndentsForKeywords: aBuilder.
			self settingsKeepBlockInMessage: aBuilder.
			self settingsAlignBlockBrackets: aBuilder.
			self settingsMethodSignatureOnMultipleLines: aBuilder.
			self settingsOneLineMessages: aBuilder.
			self settingsMultiLineMessages: aBuilder.
			self settingsMinimumNewLinesBetweenStatements: aBuilder.
			self settingsNewLineAfterCascade: aBuilder.
			self settingsNewLineBeforeFirstCascade: aBuilder.
			self settingsNewLineBeforeFirstKeyword: aBuilder.
			self settingsNewLinesAfterMethodComment: aBuilder.
			self settingsNewLinesAfterMethodSignature: aBuilder.
			self settingsNewLinesAfterTemporaries: aBuilder.
			self settingsNumberOfArgumentsForMultiLine: aBuilder.
			self settingsPeriodsAtEndOfBlock: aBuilder.
			self settingsPeriodsAtEndOfMethod: aBuilder.
			self settingsRetainBlankLinesBeforeComments: aBuilder.
			self settingsRetainBlankLinesBetweenStatements: aBuilder.
			self settingsSelectorAndArgumentCombinedMaxSize: aBuilder.
			self settingsSpacesAfterCaretSymbolInReturn: aBuilder.
			self settingsSpacesInsideBlocks: aBuilder.
			self settingsSpacesInsideParentheses: aBuilder.
			self settingsSpacesInsideArray: aBuilder.
			self settingsUseBasicCommentFormat: aBuilder]
]

{ #category : #settings }
EFFormatter class >> settingsOneLineMessages: aBuilder [

	(aBuilder setting: #oneLineMessages)
		label: 'One line messages';
		description: 'All the messages listed in this setting will be formatted on one single line. 
		
If we include #todo: in the oneLineMessages we get 

1 to: 4 do: [ :i | Transcript show: i ]'
]

{ #category : #settings }
EFFormatter class >> settingsPeriodsAtEndOfBlock: aBuilder [

	(aBuilder setting: #periodsAtEndOfBlock)
		label: 'Period at end of block body';
		description: 'This settings puts a period after the last expression of a block.

Example: when set to true

	1 to:10 do: [:i | i+1.]

'.
]

{ #category : #settings }
EFFormatter class >> settingsPeriodsAtEndOfMethod: aBuilder [

	(aBuilder setting: #periodsAtEndOfMethod)
		label: 'Period at end of method';
		description: 'This setting puts a period after the last expression of a method.
		
For example: when set to true

myMethod
	^ true.

'
]

{ #category : #settings }
EFFormatter class >> settingsRetainBlankLinesBeforeComments: aBuilder [

	(aBuilder setting: #retainBlankLinesBeforeComments)
		label: 'Retain blank lines before comments';
		description: 'Keep blank lines which are before comments. 

For example, when set to true the formatter will not change the following

	|a b|
	a :=1.
	
	"Comment"
	b :=2
'
]

{ #category : #settings }
EFFormatter class >> settingsRetainBlankLinesBetweenStatements: aBuilder [

	(aBuilder setting: #retainBlankLinesBetweenStatements)
		label: 'Retain blank lines between statements';
		description: 'Keep blank lines which are between statements.
		
For example: when set to true the formatter will not change the following: 
| a b c |
a := 1.


b := 2.

c := a + b
'
]

{ #category : #settings }
EFFormatter class >> settingsSelectorAndArgumentCombinedMaxSize: aBuilder [

	(aBuilder setting: #selectorAndArgumentCombinedMaxSize)
		label: 'Selector and argument combined in method signature max size';
		description: 'the maximum length a selector and his argument can be before the message is considered multiline
		(depends of the setting methodSignatureOnMultipleLines 
		which will write the message on multiple lines only if is set to true)
		
Example:
myMethod: argument1
	and: argument2
	is the result if the max size settings is 12 or less
	and methodSignatureOnMultipleLines is set to true'
]

{ #category : #settings }
EFFormatter class >> settingsSpacesAfterCaretSymbolInReturn: aBuilder [ 

	(aBuilder setting: #numberOfSpacesAfterCaretSymbolInReturn)
		label: 'Spaces following caret symbol in return';
		description: 'Number of spaces expressed that is put just after the caret (^) of a return expression. 
For example: ^   self is the result for 3 spaces'.
]

{ #category : #settings }
EFFormatter class >> settingsSpacesBeforeDotInDynamicArray: aBuilder [

	(aBuilder setting: #numberOfSpacesBeforeDotInDynamicArray)
		label: 'Spaces before separating period in dynamic array';
		description: 'To set the number of spaces before the separating period in dynamic array { 1here. 2})
for example, when set to one space: {1 . 2 . 3}, 

when set to zero space: {1. 2. 3}'
]

{ #category : #settings }
EFFormatter class >> settingsSpacesInsideArray: aBuilder [

	(aBuilder setting: #numberOfSpacesInsideArray)
		label: 'Spaces inside arrays #( here ) or { there }';
		description: 'Set the number of spaces after the opening and before the closing symbols of an array.
		
For example, we get the following wehn set to 3 spaces: 
#(   1 2 3   )
{   1. 2. 3.   }
'
]

{ #category : #settings }
EFFormatter class >> settingsSpacesInsideBlocks: aBuilder [

	(aBuilder setting: #numberOfSpacesInsideBlock)
		label: 'Spaces inside blocks [ here ]';
		description: 'Set the number of spaces after the opening and before the closing brackets.

For example, when set to 3 spaces we get
[   :i | Transcript show: i   ]'
]

{ #category : #settings }
EFFormatter class >> settingsSpacesInsideParentheses: aBuilder [

	(aBuilder setting: #numberOfSpacesInsideParentheses)
		label: 'Spaces inside parentheses ( here )';
		description: 'Sets the number of spaces after the opening and before the closing parentheses.
For example: (   1 + 2   ) is the result for 3 spaces'
]

{ #category : #settings }
EFFormatter class >> settingsUseBasicCommentFormat: aBuilder [

	(aBuilder setting: #useBasicCommentFormat)
		label: 'Use basic comment format';
		description: 'Leave the comment unchanged and do not resize comment.'
]

{ #category : #accessing }
EFFormatter class >> spacesAfterCaretSymbolInReturn [
	^ DefaultPrettyPrintContext spacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFFormatter class >> spacesInsideBlocks [
	^ DefaultPrettyPrintContext spacesInsideBlocks
]

{ #category : #accessing }
EFFormatter class >> spacesInsideParentheses [
	^ DefaultPrettyPrintContext spacesInsideParentheses
]

{ #category : #accessing }
EFFormatter class >> useBasicCommentFormat [
	^ DefaultPrettyPrintContext useBasicCommentFormat
]

{ #category : #accessing }
EFFormatter class >> useBasicCommentFormat: aBoolean [
	DefaultPrettyPrintContext useBasicCommentFormat: aBoolean.
	self announceASettingChange
]

{ #category : #private }
EFFormatter >> addNewLinesBeforeStatementStartingAt: anInteger [ 
	| newLines |
	newLines := self minimumNewLinesBetweenStatements 
				max: (self retainBlankLinesBetweenStatements 
						ifTrue: [ self newLinesBeforeStartingAt: anInteger ]
						ifFalse: [ 0 ]).
	newLines = 0 
		ifTrue: [ self space ] 
		ifFalse: [ self newLines: newLines ] 
]

{ #category : #private }
EFFormatter >> addSpaceIfNeededForLastArgument: aPragmaNode [
	aPragmaNode isUnary
		ifTrue: [ ^ self ].
	(self pragmaArgumentNeedsSeparator: aPragmaNode arguments last)
		ifTrue: [ self space ]
]

{ #category : #private }
EFFormatter >> basicFormatCommentFor: aComment [
	codeStream nextPutAll: (originalSource copyFrom: aComment start to: aComment stop).
]

{ #category : #private }
EFFormatter >> bracketWith: bracketString around: aBlock [
	bracketString isEmpty
		ifTrue: [ ^ aBlock value ].
	codeStream nextPut: bracketString first.
	^ aBlock
		ensure: [ codeStream nextPut: bracketString last ] 
]

{ #category : #private }
EFFormatter >> bracketWith: bracketString around: aBlock indentExtraSpaces: anInteger [
	bracketString isEmpty
		ifTrue: [ ^ aBlock value ].
	codeStream nextPut: bracketString first.
	^ [self indentExtraSpaces: anInteger  around:  aBlock]
		ensure: [ codeStream nextPut: bracketString last ] 
]

{ #category : #private }
EFFormatter >> characterSeparatorMethodSignatureFor: aMethodNode [
	^ (self needsMethodSignatureOnMultipleLinesFor: aMethodNode)
			ifTrue: [ self newLine ]
			ifFalse: [ self space ]
]

{ #category : #accessing }
EFFormatter >> codeStream [
	^ codeStream
]

{ #category : #accessing }
EFFormatter >> codeStream: anObject [
	codeStream := anObject
]

{ #category : #private }
EFFormatter >> currentLineLength [
	^ codeStream position - lineStart
]

{ #category : #'private-formatting' }
EFFormatter >> firstSeparatorShouldNotPassLine: aMessageNode [
"predicat that tell if the keyWord should be forced te be on the sameLine"

	^ aMessageNode isKeyword
				and: [ aMessageNode arguments size <= 1
						or: [ self newLineBeforeFirstKeyword not ] ]
]

{ #category : #'public interface' }
EFFormatter >> format: aParseTree [
	codeStream := self initializeCodeStream.
	originalSource := aParseTree source.
	self visitNode: aParseTree.
	^ codeStream contents
]

{ #category : #'private-formatting' }
EFFormatter >> formatArray: anArrayNode [
	| current |
	codeStream nextPutAll: self spacesInsideArray.
	(anArrayNode statements
		reject: [ :each | each class = RBLiteralValueNode ])
		ifEmpty: [ anArrayNode statements
				do: [ :each | 
					current := each.
					self visitNode: each ]
				separatedBy: [ (self isLineTooLongWithNode: current)
						ifTrue: [ codeStream nextPutAll: self spacesBeforeDotInDynamicArray; nextPut: $..
							self newLine ]
						ifFalse: [ codeStream nextPutAll: self spacesBeforeDotInDynamicArray ; nextPutAll: '. ' ] 
						"here there is something wrong since we should also be able to put a space in front"] ]
		ifNotEmpty: [ anArrayNode statements size > 1
				ifTrue: [ self
						indentAround: [ 
							"I do not get why we do not have a property to decide if we emit or not the line" 
							"with the introduction of formatArrayBody: we may remove formatSingleArrayElement"
							self
								newLine;
								formatArrayBody: anArrayNode ] ]
				ifFalse: [ self formatSingleArrayElement: anArrayNode statements first ] ].
	codeStream nextPutAll: self spacesInsideArray
]

{ #category : #'private-formatting' }
EFFormatter >> formatArrayBody: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].
	statements withIndexDo: [:statement :i | 
		self visitNode: statement.
		i < statements size
			ifTrue: [ codeStream nextPutAll: self spacesBeforeDotInDynamicArray ; nextPut: $. ].
		self formatStatementCommentsFor: statement.
		i < statements size
			ifTrue: [ self addNewLinesBeforeStatementStartingAt: (statements at: i + 1) start ] ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatBlock: aBlockNode [
	| isMultiline |
	isMultiline := self 
		willBeMultiline: aBlockNode body 
		saveIn: lookaheadCode.
	codeStream nextPutAll: self spacesInsideBlocks.
	self formatBlockArgumentsFor: aBlockNode.
	self formatBlockCommentFor: aBlockNode.
	((isMultiline or: [ self isLineTooLongWithNode: aBlockNode body ]) 
		 and: [ self shouldPassNewLineAfterHeadOfBlock: aBlockNode ]) 
		ifTrue: [ self newLine ].
	self visitSequenceNode: aBlockNode body.
	(self lineUpBlockBrackets and: [ isMultiline ])
		ifTrue: [ self newLine ]
		ifFalse: [ codeStream nextPutAll: self spacesInsideBlocks ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatBlockArgumentsFor: aBlockNode [
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do: [ :each | 
			(self isLineTooLongWithNode: each )ifTrue: [self newLine ].
			codeStream nextPut: $:.
			self visitNode: each.
			self formatCommentCloseToStatements
				ifTrue:
					[ self spaceAndFormatComments: each ].
			self space ].
	codeStream nextPutAll: '| '.
]

{ #category : #'private-formatting' }
EFFormatter >> formatBlockCommentFor: aBlockNode [
	originalSource isNil
		ifTrue: [  ^ self ].
	aBlockNode comments
		do: [ :each | 
			
			self basicFormatCommentFor: each.
			(self isNonEmptySingleLineBlock: aBlockNode)
				ifTrue: [  self space ]]
]

{ #category : #accessing }
EFFormatter >> formatCommentCloseToStatements [
	^ context formatCommentCloseToStatements
]

{ #category : #accessing }
EFFormatter >> formatCommentCloseToStatements: aBoolean [
	context formatCommentCloseToStatements: aBoolean
]

{ #category : #'private-formatting' }
EFFormatter >> formatCommentsFor: aNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aNode comments
		do:
			[ :each | 
			self basicFormatCommentFor: each.
			]
]

{ #category : #private }
EFFormatter >> formatMessageNodeInAMessageStreamFor: aMessageNode [
	| messageStream |
	messageStream := WriteStream on: (String new: 100).
	self
		with: aMessageNode selectorParts
		and: aMessageNode arguments
		do: [ :selector :arg | 
			messageStream
				nextPutAll: selector;
				space;
				nextPutAll: (self formattedSourceFor: arg withDictionary: temporaryLookAheadCode) ]
		separatedBy: [ messageStream space ].
		^ messageStream 
]

{ #category : #'private-formatting' }
EFFormatter >> formatMethodBodyFor: aMethodNode [
	self
		indentAround:
			[ 
			self newLines: self numberOfNewLinesAfterMethodSignature.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatMethodCommentFor: aMethodNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aMethodNode comments
		do:
			[ :each | 
			
			self useBasicCommentFormat 
			ifTrue: [ self basicFormatCommentFor: each ]
			ifFalse: [ 
			self resizeCommentFor: each startingAt: 0].
			self newLines: self numberOfNewLinesAfterMethodComment ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatMethodPatternFor: aMethodNode [
	aMethodNode arguments isEmpty
		ifTrue: [ codeStream nextPutAll: aMethodNode selector ]
		ifFalse: [ 
			(self needsMethodSignatureOnMultipleLinesFor: aMethodNode)
				ifTrue: [ self privateFormatMethodPatternMultiLineFor: aMethodNode ]
				ifFalse: [ self privateFormatMethodPatternMonoLineFor: aMethodNode ] ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatPragmasFor: aMethodNode [
	aMethodNode pragmas do: [ :each | self visitNode: each; newLine ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatSelectorAndArguments: aMessageNode [
	"We are formatting a message and here we are about to format the message selector and its arguments."
	
	| isMultilineMessage hasMutlineArguments multilineArgumentBeforeAnotherArgument |
	multilineArgumentBeforeAnotherArgument := self messageHasMultilineArgumentBeforeAnotherArgument: aMessageNode.
	isMultilineMessage := (self isMultiLineMessage: aMessageNode) or:[multilineArgumentBeforeAnotherArgument].
	hasMutlineArguments := self hasAMultiLineMessageArgument: aMessageNode arguments.
	self
		indent:
			(((isMultilineMessage or:[hasMutlineArguments]) and:[aMessageNode isKeyword])
				ifTrue: [ self indentsForKeywords ]
				ifFalse: [ 0 ])
		around: [ self
				formatSelectorAndArguments: aMessageNode
				firstSeparator:
					(((self firstSeparatorShouldNotPassLine: aMessageNode) and:[multilineArgumentBeforeAnotherArgument not])
						ifTrue: [ self space ]
						ifFalse: [ isMultilineMessage
								ifTrue: [ [ self newLine ] ]
								ifFalse: [ [ self space ] ] ])
				restSeparator:
					((isMultilineMessage)
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ]) ]
]

{ #category : #'private-formatting' }
EFFormatter >> formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock [
	| separatorBlock i|
	i := 0.
	separatorBlock := firstBlock.
	aMessageNode isUnary
		ifTrue: [ 
				self indentAround: [self handleLineForSelector: aMessageNode selector withSeparatorBlock:
					separatorBlock.
				codeStream nextPutAll: aMessageNode selector ]]
		ifFalse: [
			aMessageNode selectorParts with: aMessageNode arguments do: [ :selector :argument | 
				i := i + 1.
				self
					handleLineForSelector: selector
					withSeparatorBlock: separatorBlock.
				separatorBlock := restBlock.
				self
					indent:
						((((self willBeMultiline: argument saveIn: temporaryLookAheadCode) and: [ i < aMessageNode selectorParts size or: [ self newLineBefore ] ])
						or:[ self isInCascadeNode ])
							ifTrue: [ 1 ] ifFalse: [ 0 ])
					around: [ codeStream nextPutAll: selector.
						self handleLineForArgument: argument ].
				((self willBeMultiline: argument saveIn: lookaheadCode) and:[i < aMessageNode selectorParts size]) ifTrue: [ separatorBlock := self newLine ]
			]
		]
]

{ #category : #'private-formatting' }
EFFormatter >> formatSequenceNodeStatementsFor: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].
	statements withIndexDo: [:statement :i | 
		self visitNode: statement.
		(i < statements size
			or: [ 
				aSequenceNode parent
					ifNil: [ self periodsAtEndOfBlock or:[ self periodsAtEndOfMethod ] ]
					ifNotNil: [ :parent | 
						parent isBlock
							ifTrue: [ self periodsAtEndOfBlock ]
							ifFalse: [ self periodsAtEndOfMethod ] ] ])
			ifTrue: [ codeStream nextPut: $. ].
		self formatStatementCommentsFor: statement.
		i < statements size
			ifTrue: [ self addNewLinesBeforeStatementStartingAt: (statements at: i + 1) start ] ]
]

{ #category : #visiting }
EFFormatter >> formatSingleArrayElement: aRBNode [ 
	"Imagine that you have {1 factorial}, you do not want an ending period. This method is only invoked on the last array element."
	
	self visitNode: aRBNode
]

{ #category : #'private-formatting' }
EFFormatter >> formatStatementCommentsFor: aStatementNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	self formatCommentCloseToStatements
		ifFalse: [ ^ self ].
	aStatementNode statementComments
		do:
			[ :each | 
			| count |
			count := self newLinesBeforeStartingAt: each start.
			self retainBlankLinesBeforeComments & (count > 0)
				ifTrue: [ self newLines: count ]
				ifFalse: [ self space ].
			self useBasicCommentFormat
				ifTrue: [ self basicFormatCommentFor: each ]
				ifFalse: [ 
			self resizeCommentFor: each startingAt: self currentLineLength ]]
]

{ #category : #private }
EFFormatter >> formatTemporariesFor: aSequenceNode [
	aSequenceNode temporaries isEmpty
		ifTrue: [ ^ self ].
	self
		bracketWith: '|'
		around:
			[ 
			self space.
			aSequenceNode temporaries
				do:
					[ :each | 
					self visitNode: each.
					self formatCommentCloseToStatements
						ifTrue: [ self formatStatementCommentsFor: each ].
					self space ] ].
	self newLines: self numberOfNewLinesAfterTemporaries
]

{ #category : #private }
EFFormatter >> formattedSourceFor: aNode withDictionary: aDictionary [
	^ aDictionary at: aNode ifAbsentPut: [ 
		  self class new
			  indent: self indent;
			  installNewContext: context;
			  format: aNode ]
]

{ #category : #private }
EFFormatter >> handleLineForArgument: anArgument [
	| isLineTooLong |
	isLineTooLong := self isLineTooLongWithNode: anArgument.
	anArgument isBlock
		ifTrue: [((self willBeMultiline: anArgument saveIn: temporaryLookAheadCode)
						and: [ self keepBlockInMessage not ])
				ifTrue: [ self newLine ]
				ifFalse: [ self space ] ]
		ifFalse: [ (self isInCascadeNode and: [ isLineTooLong ])
				ifTrue: [ self newLine ]
				ifFalse: [ isLineTooLong
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ] ].
	self visitNode: anArgument
]

{ #category : #private }
EFFormatter >> handleLineForSelector: selector withSeparatorBlock: aBlock [
	(self isLineTooLongWithString: selector)
		ifTrue: [ self newLine ]
		ifFalse: [ aBlock value ]
]

{ #category : #private }
EFFormatter >> hasAMultiLineMessageArgument: anArgumentsCollection [
	^ anArgumentsCollection
		anySatisfy: [ :each | 
			self isInCascadeNode
				ifTrue: [ self indent: 0 around: [ self willBeMultiline: each  saveIn: temporaryLookAheadCode ] ]
				ifFalse: [ 
					self
						indent: self indentsForKeywords + 1
						around: [ self willBeMultiline: each  saveIn: temporaryLookAheadCode ] ] ]
]

{ #category : #private }
EFFormatter >> headOfBlockNotEmpty: aBlockNode [

	^ aBlockNode arguments isNotEmpty	or: [aBlockNode comments isNotEmpty]
]

{ #category : #accessing }
EFFormatter >> indent [
	^ indent
]

{ #category : #accessing }
EFFormatter >> indent: anInteger [

	indent := anInteger
]

{ #category : #private }
EFFormatter >> indent: anInteger around: aBlock [ 
	self indent: self indent + anInteger.
	^aBlock ensure: [self indent: self indent - anInteger]
]

{ #category : #private }
EFFormatter >> indentAround: aBlock [ 
	self indent: 1 around: aBlock
]

{ #category : #accessing }
EFFormatter >> indentExtraSpaces [
	^ context indentExtraSpaces
]

{ #category : #accessing }
EFFormatter >> indentExtraSpaces: anInteger [
	context indentExtraSpaces: anInteger 
]

{ #category : #private }
EFFormatter >> indentExtraSpaces: anInteger around: aBlock [
	| previousIndentExtraSpacesSize |
	previousIndentExtraSpacesSize := self indentExtraSpaces size.
	self indentExtraSpaces: anInteger + previousIndentExtraSpacesSize.
	aBlock
		ensure: [ self indentExtraSpaces: previousIndentExtraSpacesSize ]
]

{ #category : #accessing }
EFFormatter >> indentString [
	^ context indentString
]

{ #category : #accessing }
EFFormatter >> indentStyle [
	^ context indentStyle
]

{ #category : #accessing }
EFFormatter >> indentStyle: aSymbole [
	context indentStyle: aSymbole
]

{ #category : #accessing }
EFFormatter >> indentTimesRepeat: anInteger [
	
	anInteger timesRepeat: 
			[ codeStream nextPutAll: self indentString ].
	codeStream nextPutAll: self indentExtraSpaces
]

{ #category : #accessing }
EFFormatter >> indentsForKeywords [
	^ context indentsForKeywords
]

{ #category : #accessing }
EFFormatter >> indentsForKeywords: anInteger [
	context indentsForKeywords: anInteger
]

{ #category : #initialization }
EFFormatter >> initialize [
	super initialize.
	lineStart := 0.
	self indent: 0.
	self isInCascadeNode: false.
	lookaheadCode := self initializeLookAheadCode.
	temporaryLookAheadCode := self initializeLookAheadCode.
	codeStream := self initializeCodeStream.
	context := DefaultPrettyPrintContext
]

{ #category : #initialization }
EFFormatter >> initializeCodeStream [
	^ WriteStream on: (String new: 256)
]

{ #category : #initialization }
EFFormatter >> initializeLookAheadCode [
	^ IdentityDictionary new
]

{ #category : #'public interface' }
EFFormatter >> installNewContext: aContext [

	context := aContext.
	lookaheadCode := self initializeLookAheadCode
]

{ #category : #accessing }
EFFormatter >> isInCascadeNode [
	^ isInCascadeNode
]

{ #category : #accessing }
EFFormatter >> isInCascadeNode: aBoolean [
	isInCascadeNode := aBoolean
]

{ #category : #private }
EFFormatter >> isLineTooLongWithNode: aNode [
	^ self isLineTooLongWithString: (self formattedSourceFor: aNode withDictionary: temporaryLookAheadCode ).
]

{ #category : #private }
EFFormatter >> isLineTooLongWithString: aString [
	^ self currentLineLength
		+ (aString indexOf: Character cr ifAbsent: [ aString size ])
		>= self maxLineLength
]

{ #category : #private }
EFFormatter >> isMultiLineMessage: aMessageNode [
	(self multiLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ true ].
	(self oneLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ false ].
	self numberOfArgumentsForMultiLine <= aMessageNode arguments size
		ifTrue: [ ^ true ].
	aMessageNode isUnary
		ifTrue: [ ^ self isLineTooLongWithString: aMessageNode selector ].
	^ self isLineTooLongWithString: (self formatMessageNodeInAMessageStreamFor: aMessageNode) contents
]

{ #category : #private }
EFFormatter >> isNonEmptySingleLineBlock: aBlockNode [
	^ ((self willBeMultiline: aBlockNode body saveIn: temporaryLookAheadCode ) or: [ aBlockNode body statements isEmpty ]) not
]

{ #category : #accessing }
EFFormatter >> keepBlockInMessage [
	"Return a boolean for 
		1 to: 4 do: [ :i | 
			
	vs. 
	
		1 to: 4 do: 
			[ :i |"
			
	^ context keepBlockInMessage
]

{ #category : #accessing }
EFFormatter >> keepBlockInMessage: aBoolean [
	context keepBlockInMessage: aBoolean
]

{ #category : #accessing }
EFFormatter >> lineStart [
	^ lineStart
]

{ #category : #accessing }
EFFormatter >> lineStart: anObject [
	lineStart := anObject
]

{ #category : #accessing }
EFFormatter >> lineUpBlockBrackets [
	"Return whether a new line should be added before closing a multiline block.
	1 to: 4 do: [ Transcript
		show: i;
		cr
	]
	"
	^ context lineUpBlockBrackets
]

{ #category : #accessing }
EFFormatter >> lineUpBlockBrackets: aBoolean [
	context lineUpBlockBrackets: aBoolean
]

{ #category : #accessing }
EFFormatter >> maxLineLength [
	^ context maxLineLength
]

{ #category : #accessing }
EFFormatter >> maxLineLength: anInteger [
	context maxLineLength: anInteger
]

{ #category : #private }
EFFormatter >> messageHasMultilineArgumentBeforeAnotherArgument: aMessageNode [ 

	aMessageNode arguments doWithIndex: [ :each :i |
		(self willBeMultiline: each saveIn: temporaryLookAheadCode)
			ifTrue: [ ^ i < aMessageNode arguments size ]].
	^ false
]

{ #category : #accessing }
EFFormatter >> methodSignatureOnMultipleLines [
	^ context methodSignatureOnMultipleLines
]

{ #category : #accessing }
EFFormatter >> methodSignatureOnMultipleLines: aBoolean [
	context methodSignatureOnMultipleLines: aBoolean
]

{ #category : #accessing }
EFFormatter >> minimumNewLinesBetweenStatements [
	^ context minimumNewLinesBetweenStatements
]

{ #category : #accessing }
EFFormatter >> minimumNewLinesBetweenStatements: anInteger [
	context minimumNewLinesBetweenStatements: anInteger
]

{ #category : #accessing }
EFFormatter >> multiLineMessages [
	^ context multiLineMessages
]

{ #category : #accessing }
EFFormatter >> multiLineMessages: anArray [
	context multiLineMessages: anArray
]

{ #category : #private }
EFFormatter >> needsMethodSignatureOnMultipleLinesFor: aMethodNode [
	| cpt |
	cpt := 0.
	self methodSignatureOnMultipleLines
		ifTrue:
			[ 
			aMethodNode selectorParts
				with: aMethodNode arguments
				do:
					[ :key :arg | 
					key size + arg name size > self selectorAndArgumentCombinedMaxSize
						ifTrue: [ cpt := cpt + 1 ] ].
			^ cpt > 1 ]
		ifFalse: [ ^ false ]
]

{ #category : #private }
EFFormatter >> needsParenthesisFor: aNode [
	| parent grandparent |
	aNode ifNil: [ ^ false ].
	aNode isValue
		ifFalse: [ ^ false ].
	aNode isParseError
		ifTrue: [ ^ false ].
	parent := aNode parent ifNil: [ ^ false ].
	(aNode isMessage
		and: [ parent isMessage
				and: [ parent receiver == aNode and: [ aNode selector isUnary not ] ] ])
		ifTrue: [ grandparent := parent parent.
			(grandparent notNil and: [ grandparent isCascade ])
				ifTrue: [ ^ true ] ].
	aNode precedence < parent precedence
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ parent isAssignment ])
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ aNode isCascade ])
		ifTrue: [ ^ true ].
	aNode precedence = 0
		ifTrue: [ ^ false ].
	aNode isMessage
		ifFalse: [ ^ true ].
	aNode isUnary
		ifTrue: [ ^ false ].
	aNode isKeyword
		ifTrue: [ ^ true ].
	(parent isMessage and: [ parent receiver == aNode ])
		ifFalse: [ ^ true ].
	aNode precedence = parent precedence
		ifFalse: [ ^ true ].
	^ false
]

{ #category : #private }
EFFormatter >> newLine [
	self newLines: 1
]

{ #category : #accessing }
EFFormatter >> newLineAfterCascade [
	^ context newLineAfterCascade
]

{ #category : #accessing }
EFFormatter >> newLineAfterCascade: aBoolean [
	context newLineAfterCascade: aBoolean
]

{ #category : #private }
EFFormatter >> newLineBefore [
	"predicate which return true if there where a newLine in the code stream just before the current position"

	| currentIndentationString |
	currentIndentationString := WriteStream on: (String new: self maxLineLength).
	1 to: self indent do: [ :i | 
		currentIndentationString nextPutAll: self indentString ].
	currentIndentationString nextPutAll: self indentExtraSpaces.
	^ self codeStream contents lines last = currentIndentationString contents
]

{ #category : #accessing }
EFFormatter >> newLineBeforeFirstCascade [
	^ context newLineBeforeFirstCascade
]

{ #category : #accessing }
EFFormatter >> newLineBeforeFirstCascade: aBoolean [
	context newLineBeforeFirstCascade: aBoolean
]

{ #category : #accessing }
EFFormatter >> newLineBeforeFirstKeyword [
	^ context newLineBeforeFirstKeyword
]

{ #category : #accessing }
EFFormatter >> newLineBeforeFirstKeyword: aBoolean [
	context newLineBeforeFirstKeyword: aBoolean
]

{ #category : #private }
EFFormatter >> newLines: anInteger [
	anInteger + self indentString size = 0
		ifTrue: 
			[ codeStream space ].
	anInteger
		timesRepeat: 
			[ codeStream cr ].
	lineStart := codeStream position.
	self
		indentTimesRepeat: self indent
]

{ #category : #private }
EFFormatter >> newLinesBeforeStartingAt: anIndex [
	| count cr lf index char |
	originalSource ifNil: [ ^ 0 ].
	(anIndex isNil or: [ anIndex > originalSource size ])
		ifTrue: [ ^ 0 ].
	cr := Character value: 13.
	lf := Character value: 10.
	count := 0.
	index := anIndex - 1.
	[ index > 0 and: [ (char := originalSource at: index) isSeparator ] ]
		whileTrue: [ 
			char == lf
				ifTrue: [ 
					count := count + 1.
					(originalSource at: (index - 1 max: 1)) == cr
						ifTrue: [ index := index - 1 ] ].
			char == cr
				ifTrue: [ count := count + 1 ].
			index := index - 1 ].
	^ count
]

{ #category : #accessing }
EFFormatter >> numberOfArgumentsForMultiLine [
	^ context numberOfArgumentsForMultiLine
]

{ #category : #accessing }
EFFormatter >> numberOfArgumentsForMultiLine: anInteger [
	context numberOfArgumentsForMultiLine: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterMethodComment [
	^ context numberOfNewLinesAfterMethodComment
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterMethodComment: anInteger [
	context numberOfNewLinesAfterMethodComment: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterMethodSignature [
	^ context numberOfNewLinesAfterMethodSignature
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterMethodSignature: anInteger [
	context numberOfNewLinesAfterMethodSignature: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterTemporaries [
	^ context numberOfNewLinesAfterTemporaries
]

{ #category : #accessing }
EFFormatter >> numberOfNewLinesAfterTemporaries: anInteger [
	context numberOfNewLinesAfterTemporaries: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesAfterCaretSymbolInReturn [
	^ context numberOfSpacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesAfterCaretSymbolInReturn: anInteger [
	context numberOfSpacesAfterCaretSymbolInReturn: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInIndent [
	^ context numberOfSpacesInIndent
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInIndent: anInteger [
	context numberOfSpacesInIndent: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideArray [
	^ context numberOfSpacesInsideArray
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideArray: anInteger [
	context numberOfSpacesInsideArray: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideBlock [
	^ context numberOfSpacesInsideBlock
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideBlock: anInteger [
	context numberOfSpacesInsideBlock: anInteger
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideParentheses [
	^ context numberOfSpacesInsideParentheses
]

{ #category : #accessing }
EFFormatter >> numberOfSpacesInsideParentheses: anInteger [
	context numberOfSpacesInsideParentheses: anInteger
]

{ #category : #accessing }
EFFormatter >> oneLineMessages [
	^ context oneLineMessages
]

{ #category : #accessing }
EFFormatter >> oneLineMessages: anArray [
	context oneLineMessages: anArray
]

{ #category : #accessing }
EFFormatter >> periodAtEndOfBlock: aBoolean [
	context periodAtEndOfBlock: aBoolean
]

{ #category : #accessing }
EFFormatter >> periodAtEndOfMethod: aBoolean [
	context periodAtEndOfMethod: aBoolean
]

{ #category : #accessing }
EFFormatter >> periodsAsTerminators [
	^ context periodsAtEndOfBlock and: [ context periodsAtEndOfMethod ]
]

{ #category : #accessing }
EFFormatter >> periodsAsTerminators: aBoolean [
	context periodAtEndOfBlock: aBoolean.
	context periodAtEndOfMethod: aBoolean
]

{ #category : #accessing }
EFFormatter >> periodsAtEndOfBlock [
	^ context periodsAtEndOfBlock
]

{ #category : #accessing }
EFFormatter >> periodsAtEndOfMethod [
	^ context periodsAtEndOfMethod
]

{ #category : #private }
EFFormatter >> pragmaArgumentNeedsSeparator: anArgumentNode [
	^ anArgumentNode value isSymbol and: [ anArgumentNode value isBinary ]
]

{ #category : #private }
EFFormatter >> privateFormatMethodPatternMonoLineFor: aMethodNode [
	self
		with: aMethodNode selectorParts
		and: aMethodNode arguments
		do: [ :key :arg | 
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self characterSeparatorMethodSignatureFor: aMethodNode ]
]

{ #category : #private }
EFFormatter >> privateFormatMethodPatternMultiLineFor: aMethodNode [
	| selectors arguments |
	arguments := aMethodNode arguments.
	selectors := aMethodNode selectorParts.
	codeStream nextPutAll: selectors first.
	self space.
	self visitNode: arguments first.
	codeStream nextPut: Character cr.
	self
		with: selectors allButFirst
		and: arguments allButFirst
		do: [ :key :arg | 
			codeStream nextPutAll: self indentString.
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self characterSeparatorMethodSignatureFor: aMethodNode].
	self newLines: 1
]

{ #category : #utilities }
EFFormatter >> resizeComment: aComment withFirstLineShorterOf: anIndex [
	| cutComment firstLine currentCharPos |
	currentCharPos := 1.
	firstLine := (aComment contents
		withNoLineLongerThan: self maxLineLength - anIndex) lineNumber: 1.
	[ currentCharPos > firstLine size ]
		whileFalse: [ codeStream nextPut: (firstLine at: currentCharPos).
			currentCharPos := currentCharPos + 1 ].
	currentCharPos > aComment contents size
		ifTrue: [ ^ self ].
	self newLine.
	cutComment := aComment contents allButFirst: currentCharPos.
	self resizeStringDependingOnWindowSizeFor: cutComment
]

{ #category : #utilities }
EFFormatter >> resizeCommentFor: aComment startingAt: anIndex [
	codeStream nextPut: $".
	"Hack to keep the right comment in pop up like cmd n, cmd m..."
	self maxLineLength < 50
		ifTrue: [ codeStream nextPutAll: aComment contents ]
		ifFalse:
			[ 
			aComment contents
				ifNotEmpty:
					[ 
					(anIndex = 0 or: [ anIndex >= self maxLineLength ])
						ifTrue: [ self resizeStringDependingOnWindowSizeFor: aComment ]
						ifFalse: [ self resizeComment: aComment withFirstLineShorterOf: anIndex ] ] ].
	codeStream nextPut: $"
]

{ #category : #utilities }
EFFormatter >> resizeStringDependingOnWindowSizeFor: aComment [
	| resizedComment |
	resizedComment := aComment contents withNoLineLongerThan: self maxLineLength.
	resizedComment do: [ :each | 
		codeStream nextPut: each.
		each = Character cr
			ifTrue: [ self indentTimesRepeat: self indent  ] ]
]

{ #category : #accessing }
EFFormatter >> retainBlankLinesBeforeComments [
	^ context retainBlankLinesBeforeComments
]

{ #category : #accessing }
EFFormatter >> retainBlankLinesBeforeComments: aBoolean [
	context retainBlankLinesBeforeComments: aBoolean
]

{ #category : #accessing }
EFFormatter >> retainBlankLinesBetweenStatements [
	^ context retainBlankLinesBetweenStatements
]

{ #category : #accessing }
EFFormatter >> retainBlankLinesBetweenStatements: aBoolean [
	context retainBlankLinesBetweenStatements: aBoolean
]

{ #category : #accessing }
EFFormatter >> selectorAndArgumentCombinedMaxSize [
	^ context selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
EFFormatter >> selectorAndArgumentCombinedMaxSize: anInteger [
	context selectorAndArgumentCombinedMaxSize: anInteger
]

{ #category : #'private-formatting' }
EFFormatter >> shouldPassNewLineAfterHeadOfBlock: aBlockNode [
	^ (self headOfBlockNotEmpty: aBlockNode)
		or: [ self keepBlockInMessage ]
]

{ #category : #private }
EFFormatter >> space [
	codeStream space
]

{ #category : #'private-formatting' }
EFFormatter >> spaceAndFormatComments: aNode [
	aNode comments ifNotEmpty: [ self space ].
	self formatCommentsFor: aNode
]

{ #category : #accessing }
EFFormatter >> spacesAfterCaretSymbolInReturn [
	^ context spacesAfterCaretSymbolInReturn
]

{ #category : #accessing }
EFFormatter >> spacesBeforeDotInDynamicArray [ 
	^ context spacesBeforeDotInDynamicArray
]

{ #category : #accessing }
EFFormatter >> spacesInsideArray [
	^ context spacesInsideArray
]

{ #category : #accessing }
EFFormatter >> spacesInsideBlocks [
	^ context spacesInsideBlocks
]

{ #category : #accessing }
EFFormatter >> spacesInsideParentheses [
	^ context spacesInsideParentheses
]

{ #category : #accessing }
EFFormatter >> useBasicCommentFormat [ 
	^ context useBasicCommentFormat 
]

{ #category : #accessing }
EFFormatter >> useBasicCommentFormat: aBoolean [
	context useBasicCommentFormat: aBoolean
]

{ #category : #visiting }
EFFormatter >> visitArrayNode: anArrayNode [
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]
]

{ #category : #visiting }
EFFormatter >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self
		indentExtraSpaces: anAssignmentNode variable name size + ' := ' size
		around: [ self visitNode: anAssignmentNode value ]
]

{ #category : #visiting }
EFFormatter >> visitBlockNode: aBlockNode [
	self
		bracketWith: '[]'
		around: [ self formatBlock: aBlockNode ].
	
]

{ #category : #visiting }
EFFormatter >> visitCascadeNode: aCascadeNode [
	self isInCascadeNode: true.
	self visitNode: aCascadeNode receiver.
	self
		indentAround: [ 
			self newLineBeforeFirstCascade
				ifTrue: [ self newLine ]
				ifFalse: [ self space ].
			aCascadeNode messages
				do: [ :each | 
							self formatCommentsFor: each;
								formatSelectorAndArguments: each
								firstSeparator: [  ]
								restSeparator:
									((self isMultiLineMessage: each)
										ifTrue: [ [ self newLine ] ]
										ifFalse: [ [ self space ] ])]
				separatedBy: [ 
					codeStream nextPut: $;.
					self newLineAfterCascade
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ]
					].
	self isInCascadeNode: false
]

{ #category : #visiting }
EFFormatter >> visitLiteralArrayNode: aRBArrayLiteralNode [
	| brackets current |
	codeStream nextPut: $#.
	brackets := aRBArrayLiteralNode isForByteArray
		ifTrue: [ '[]' ]
		ifFalse: [ '()' ].
	self
		bracketWith: brackets
		around: [ self
				indentExtraSpaces: '#(' size + self numberOfSpacesInsideArray
				around: [ codeStream nextPutAll: self spacesInsideArray.
					aRBArrayLiteralNode contents
						do: [ :each | 
							current := each.
							self visitNode: each ]
						separatedBy: [ (self isLineTooLongWithNode: current)
								ifTrue: [ self newLine ]
								ifFalse: [ self space ] ].
					codeStream nextPutAll: self spacesInsideArray ] ]
]

{ #category : #visiting }
EFFormatter >> visitLiteralNode: aLiteralNode [
	aLiteralNode value isLiteral
		ifFalse: [ self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString: aLiteralNode sourceText ]
]

{ #category : #visiting }
EFFormatter >> visitLiteralVariableNode: aLiteralVariableNode [
	| binding |
	binding := aLiteralVariableNode binding.
	self writeString: binding key storeString, ' -> '.
	aLiteralVariableNode binding value isLiteral
		ifFalse: [ self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString:  binding value asString ]
]

{ #category : #visiting }
EFFormatter >> visitMessageNode: aMessageNode [
	self visitNode: aMessageNode receiver.
	self formatSelectorAndArguments: aMessageNode
]

{ #category : #visiting }
EFFormatter >> visitMethodNode: aMethodNode [ 
	self formatMethodPatternFor: aMethodNode.
	self formatMethodBodyFor: aMethodNode
]

{ #category : #visiting }
EFFormatter >> visitNode: aNode [
	| needsParenthesis |
	(lookaheadCode includesKey: aNode)
		ifTrue: [ ^ self writeString: (lookaheadCode at: aNode) ].
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: [ '()' ]
				ifFalse: [ '' ])
		around: [ 
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self spacesInsideParentheses ].
			super visitNode: aNode.
			(self formatCommentCloseToStatements or: [ aNode isMethod or: [ aNode isSequence or: [ aNode isBlock ] ] ])
				ifFalse: [  self spaceAndFormatComments: aNode. ].
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self spacesInsideParentheses ] ]
		indentExtraSpaces: (needsParenthesis ifTrue: [ 1 + self numberOfSpacesInsideParentheses ] ifFalse:[ 0 ])
]

{ #category : #visiting }
EFFormatter >> visitParseErrorNode: aNode [
	self writeString: aNode value
]

{ #category : #visiting }
EFFormatter >> visitPatternBlockNode: aRBPatternBlockNode [ 
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternBlockNode]
]

{ #category : #visiting }
EFFormatter >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternWrapperBlockNode]
]

{ #category : #visiting }
EFFormatter >> visitPragmaNode: aPragmaNode [
	codeStream nextPut: $<.
	self 
		formatSelectorAndArguments: aPragmaNode 
		firstSeparator: [ 
			aPragmaNode selector isInfix
				ifTrue: [ self space ] ]
		restSeparator: [ self space ].
	self addSpaceIfNeededForLastArgument: aPragmaNode.
	codeStream nextPut: $>
]

{ #category : #visiting }
EFFormatter >> visitReturnNode: aReturnNode [
	codeStream
		nextPut: $^;
		nextPutAll: self spacesAfterCaretSymbolInReturn.
	self
		indentExtraSpaces: 1 + self numberOfSpacesAfterCaretSymbolInReturn
		around: [ self visitNode: aReturnNode value ]
]

{ #category : #visiting }
EFFormatter >> visitSequenceNode: aSequenceNode [
	self formatTemporariesFor: aSequenceNode.
	aSequenceNode comments
		ifNotEmpty: [ self formatCommentsFor: aSequenceNode;
						newLine ].
	self formatSequenceNodeStatementsFor: aSequenceNode
]

{ #category : #visiting }
EFFormatter >> visitVariableNode: aVariableNode [ 
	codeStream nextPutAll: aVariableNode name
]

{ #category : #private }
EFFormatter >> willBeMultiline: aNode [ 
	^(self formattedSourceFor: aNode withDictionary: lookaheadCode) includes: Character cr
]

{ #category : #private }
EFFormatter >> willBeMultiline: aNode saveIn: aDictionary [
	^(self formattedSourceFor: aNode withDictionary: aDictionary) includes: Character cr
]

{ #category : #utilities }
EFFormatter >> with: firstCollection and: secondCollection do: aBlock separatedBy: separatorBlock [
	firstCollection isEmpty
		ifTrue: [ ^ self ].
	aBlock value: firstCollection first value: secondCollection first.
	2 to: firstCollection size do: [ :i | 
		separatorBlock value.
		aBlock value: (firstCollection at: i) value: (secondCollection at: i) ]
]

{ #category : #private }
EFFormatter >> writeString: aString [
	| index |
	index := aString lastIndexOf: Character cr ifAbsent: [ 0 ].
	 codeStream nextPutAll: aString .
	index > 0
		ifTrue: [ lineStart := codeStream position - (aString size - index) ]
]
