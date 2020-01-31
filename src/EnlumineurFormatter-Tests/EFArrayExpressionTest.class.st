Class {
	#name : #EFArrayExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFArrayExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		numberOfSpacesInsideParentheses: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 0.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #configurations }
EFArrayExpressionTest >> fiveSpacesInsideArrayConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		numberOfSpacesInsideParentheses: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideArray: 5;
		numberOfSpacesBeforeDotInDynamicArray: 0.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #configurations }
EFArrayExpressionTest >> maxLineLengthConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 12;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		indentExtraSpaces: 0;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 0.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #configurations }
EFArrayExpressionTest >> maxLineLengthConfigurationWithSpaceBefore [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 12;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		indentExtraSpaces: 0;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 1.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #configurations }
EFArrayExpressionTest >> multiLineConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		numberOfSpacesInsideParentheses: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideParentheses: 0;
		multiLineMessages: #();
		oneLineMessages: #();
		numberOfArgumentsForMultiLine: 4;
		indentsForKeywords: 1;
		keepBlockInMessage: true;
		indentExtraSpaces: 0;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 0
]

{ #category : #configurations }
EFArrayExpressionTest >> oneSpaceInsideArrayConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		numberOfSpacesInsideParentheses: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideArray: 1;
		numberOfSpacesBeforeDotInDynamicArray: 0.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #configurations }
EFArrayExpressionTest >> periodAtEndOfMethod [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 12;
		indentsForKeywords: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: true;
		multiLineMessages: #();
		oneLineMessages: #();
		numberOfSpacesInsideParentheses: 0;
		numberOfArgumentsForMultiLine: 2;
		newLineBeforeFirstKeyword: false;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 0
]

{ #category : #configurations }
EFArrayExpressionTest >> spaceInsideArrayConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideArray: 5;
		numberOfSpacesBeforeDotInDynamicArray: 0.
		"here since we shortcut the initialize method we have to set the numberOfSpace...
		manually so that the spacesBefore string is correctly computed." 
]

{ #category : #tests }
EFArrayExpressionTest >> testEmptyArray [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{}'.
	self assert: source equals: '{}'
]

{ #category : #tests }
EFArrayExpressionTest >> testFiveSpaceAroundBrackets [
	| source |
	configurationSelector := #fiveSpacesInsideArrayConfiguration.
	source := self formatExpression: ' { 1. 2. 3 } '.
	self assert: source equals: '{     1. 2. 3     }'
]

{ #category : #tests }
EFArrayExpressionTest >> testFormatElement [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' { a:=1. b:=2 } '.
	self assert: source equals:
'{
	(a := 1).
	(b := 2)}'
]

{ #category : #tests }
EFArrayExpressionTest >> testNewLineAfterBracesWhenMultiline [
	| source |
	configurationSelector := #multiLineConfiguration.
	source := self formatExpression: '{(self + (1 @ 0)).
	(self + (1 @ 1)).
	(self + (0 @ 1)).
	(self + (-1 @ 1)).
	(self + (-1 @ 0)).
	(self + (-1 @ -1)).
	(self + (0 @ -1)).
	(self + (1 @ -1))}'.
	self
		assert: source
		equals:
'{
	(self + (1 @ 0)).
	(self + (1 @ 1)).
	(self + (0 @ 1)).
	(self + (-1 @ 1)).
	(self + (-1 @ 0)).
	(self + (-1 @ -1)).
	(self + (0 @ -1)).
	(self + (1 @ -1))}'
]

{ #category : #tests }
EFArrayExpressionTest >> testNoExtraSpaceAroundPeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{1.    2.        3}'.
	self assert: source equals: '{1. 2. 3}'
]

{ #category : #tests }
EFArrayExpressionTest >> testNoSpaceAroundBrackets [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' { 1. 2. 3 } '.
	self assert: source equals: '{1. 2. 3}'
]

{ #category : #tests }
EFArrayExpressionTest >> testOneSpaceAroundBrackets [
	| source |
	configurationSelector := #oneSpaceInsideArrayConfiguration.
	source := self formatExpression: ' { 1. 2. 3 } '.
	self assert: source equals: '{ 1. 2. 3 }'
]

{ #category : #tests }
EFArrayExpressionTest >> testPeriodAtEndOfMethod [
	| source |
	configurationSelector := #periodAtEndOfMethod.
	source := self formatExpression: '{1 factorial}'.
	self assert: source equals: '{1 factorial}'
]

{ #category : #tests }
EFArrayExpressionTest >> testPeriodAtEndOfMethodWithMultipleElementArrayShouldNotHavePeriodAtEnd [
	| source |
	configurationSelector := #periodAtEndOfMethod.
	source := self formatExpression: '{ a := 2 . a := 2 }'.
	self assert: source equals:  '{
	(a := 2).
	(a := 2)}'
]

{ #category : #tests }
EFArrayExpressionTest >> testPeriodAtEndOfMethodWithMultipleElementArrayShouldNotHavePointAtTheEnd [
	| source |
	configurationSelector := #periodAtEndOfMethod.
	source := self formatExpression: '{1 factorial . 3 + 2}'.
	self assert: source equals: 
 '{
	1 factorial.
	(3 + 2)}' "we should not have a point at the end."	
]

{ #category : #tests }
EFArrayExpressionTest >> testSpaceAroundPeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{1. 2. 3}'.
	self assert: source equals: '{1. 2. 3}'
]

{ #category : #tests }
EFArrayExpressionTest >> testSpaceBeforePeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{1 . 2 . 3}'.
	self assert: source equals: '{1. 2. 3}'
]

{ #category : #tests }
EFArrayExpressionTest >> testSpaceInside [
	| source |
	configurationSelector := #spaceInsideArrayConfiguration.
	source := self formatExpression: ' { 1. 2. 3 } '.
	self assert: source equals: '{     1. 2. 3     }'
]

{ #category : #tests }
EFArrayExpressionTest >> testThreeSpacesBeforePeriod [
	| source |
	configurationSelector := #threeSpacesBeforeDotInArrayConfiguration.
	source := self formatExpression: '{1. 2. 3}'.
	self assert: source equals: '{1   . 2   . 3}'
]

{ #category : #tests }
EFArrayExpressionTest >> testToLongNewLine [
	| source |
	configurationSelector := #maxLineLengthConfiguration.
	source := self formatExpression: '{VeryLongExpression. 1}'.
	self
		assert: source
		equals:
'{
	VeryLongExpression.
	1}'
]

{ #category : #tests }
EFArrayExpressionTest >> testToLongNewLine2 [
	| source |
	configurationSelector := #maxLineLengthConfiguration.
	source := self formatExpression: '{1. 2. 3. 4. 5. 6. 7. 1}'.
	self
		assert: source
		equals:
			'{1. 2. 3. 4.
5. 6. 7. 1}'
]

{ #category : #tests }
EFArrayExpressionTest >> testToLongNewLine2WithSpaceBefore [
	| source |
	configurationSelector := #maxLineLengthConfigurationWithSpaceBefore.
	source := self formatExpression: '{1. 2. 3. 4. 5. 6. 7. 1}'.
	self
		assert: source
		equals:
			'{1 . 2 . 3 . 4 .
5 . 6 . 7 . 1}'
]

{ #category : #tests }
EFArrayExpressionTest >> testToLongNewLineWithSpaceBefore [
	| source |
	configurationSelector := #maxLineLengthConfigurationWithSpaceBefore.
	source := self formatExpression: '{VeryLongExpression. 1}'.
	self
		assert: source
		equals:
'{
	VeryLongExpression .
	1}'
]

{ #category : #configurations }
EFArrayExpressionTest >> threeSpacesBeforeDotInArrayConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 50;
		numberOfSpacesInsideParentheses: 0;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		indentStyle: #tabulation;
		indentExtraSpaces: 0;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfSpacesInsideArray: 0;
		numberOfSpacesBeforeDotInDynamicArray: 3
]
