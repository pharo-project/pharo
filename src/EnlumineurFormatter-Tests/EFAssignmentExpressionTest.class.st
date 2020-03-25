Class {
	#name : #EFAssignmentExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFAssignmentExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		maxLineLength: 70;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFAssignmentExpressionTest >> multilineConfiguration [
	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		maxLineLength: 70;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 4;
		indentsForKeywords: 1;
		keepBlockInMessage: true;
		numberOfSpacesInsideParentheses: 1;
		indentStyle: #tabulation;
		retainBlankLinesBetweenStatements: true;
		minimumNewLinesBetweenStatements: 1;
		periodAtEndOfMethod: false;
		periodAtEndOfBlock: false;
		numberOfSpacesInsideBlock: 1;
		lineUpBlockBrackets: true;
		newLineBeforeFirstKeyword: true;
		indentExtraSpaces: 0
]

{ #category : #test }
EFAssignmentExpressionTest >> testAssignment [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=1'.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testAssignmentFormatValue [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:= b:=1'.
	self assert: source equals: 'a := b := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testAssignmentFormatVariable [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' a   := 1'.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testAssignmentHasNoExtraSpace [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a:=1   '.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testAssignmentHasNoExtraSpaceInside [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a    :=    1   '.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testExtraIndentationWhenKeywordMessageIsMultiline [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self
		formatExpression:
'abcdefghijklmnop:= aBoolean
	ifTrue: [ self indentsForKeywords ]
	ifFalse: [ 0 ]'.
	self
		assert: source
		equals:
'abcdefghijklmnop := aBoolean
	                    ifTrue: [ self indentsForKeywords ]
	                    ifFalse: [ 0 ]'
]

{ #category : #test }
EFAssignmentExpressionTest >> testExtraIndentationWhenMultiline [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self
		formatExpression:
			'abcdefghijklmnop:= p2-x - p0-x * ( p3-y - p0-y ) - ( p3-x - p0-x * ( p2-y - p0-y ) ) * b0'.
	self
		assert: source
		equals:
'abcdefghijklmnop := p2 - x - p0 - x * ( p3 - y - p0 - y )
                    - ( p3 - x - p0 - x * ( p2 - y - p0 - y ) ) * b0'
]

{ #category : #test }
EFAssignmentExpressionTest >> testExtraIndentationWhenMultiline2 [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self
		formatExpression:
			'1 to: 4 do: [a:= p2-x - p0-x * ( p3-y - p0-y ) - ( p3-x - p0-x * ( p2-y - p0-y ) )
	  * b0]'.
	self
		assert: source
		equals:
'1 to: 4 do: [ 
	a := p2 - x - p0 - x * ( p3 - y - p0 - y )
	     - ( p3 - x - p0 - x * ( p2 - y - p0 - y ) ) * b0
	]'
]

{ #category : #test }
EFAssignmentExpressionTest >> testExtraIndentationWhenMultiline3 [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self
		formatExpression:
	'a:= b := p2-x - p0-x * ( p3-y - p0-y ) - ( p3-x - p0-x * ( p2-y - p0-y ) )
	  * b0'.
	self
		assert: source
		equals:
'a := b := p2 - x - p0 - x * ( p3 - y - p0 - y )
          - ( p3 - x - p0 - x * ( p2 - y - p0 - y ) ) * b0'

]

{ #category : #test }
EFAssignmentExpressionTest >> testExtraIndentationWhenMultilineResetTheIndentWhenFinish [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self formatExpression: 'a := p2-x - p0-x * ( p3-y - p0-y ) - ( p3-x - p0-x * ( p2-y - p0-y ) ) * b0. a:=1'.
	self assert: source equals:
'a := p2 - x - p0 - x * ( p3 - y - p0 - y )
     - ( p3 - x - p0 - x * ( p2 - y - p0 - y ) ) * b0.
a := 1'
]

{ #category : #test }
EFAssignmentExpressionTest >> testMultipleAssignmentHasNoExtraSpaceInside [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a    :=    b :=  1   '.
	self assert: source equals: 'a := b := 1'
]
