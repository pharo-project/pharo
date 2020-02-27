Class {
	#name : #EFReturnExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configuration }
EFReturnExpressionTest >> basicConfiguration [
	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		numberOfSpacesAfterCaretSymbolInReturn: 1;
		maxLineLength: 70;
		indentExtraSpaces: 0
]

{ #category : #configuration }
EFReturnExpressionTest >> multilineConfiguration [
	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		numberOfSpacesAfterCaretSymbolInReturn: 1;
		maxLineLength: 70;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 4;
		indentStyle: #tabulation;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		indentExtraSpaces: 0
]

{ #category : #configuration }
EFReturnExpressionTest >> multilineWithThreeSpacesAfterCaretSymbolConfiguration [
	^ self contextClass basicNew
		formatCommentCloseToStatements: false;
		numberOfSpacesAfterCaretSymbolInReturn: 3;
		maxLineLength: 70;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 4;
		indentStyle: #tabulation;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: true;
		keepBlockInMessage: true;
		indentExtraSpaces: 0
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnExtraSpacesInIndentWhenMultilne [
	| source |
	configurationSelector := #multilineConfiguration.
	source := self formatExpression: '^ Array with: self leftRotated with: self negated with: self rightRotated with: self'.
	self assert: source equals:
'^ Array
	  with: self leftRotated
	  with: self negated
	  with: self rightRotated
	  with: self'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnExtraSpacesInIndentWhenMultilneWithThreeSpacesAfterCaretSymbol [
	| source |
	configurationSelector := #multilineWithThreeSpacesAfterCaretSymbolConfiguration.
	source := self formatExpression: '^ Array with: self leftRotated with: self negated with: self rightRotated with: self'.
	self assert: source equals:
'^   Array
	    with: self leftRotated
	    with: self negated
	    with: self rightRotated
	    with: self'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnFormatValue [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    ^a:=1   '.
	self assert: source equals: '^ a := 1'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnHasNoExtraSpace [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    ^a   '.
	self assert: source equals: '^ a'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnHasNoExtraSpaceInside [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' ^    a'.
	self assert: source equals: '^ a'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnHasNoPeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '^a.'.
	self assert: source equals: '^ a'
]

{ #category : #tests }
EFReturnExpressionTest >> testReturnHasSpaceBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '^a'.
	self assert: source equals: '^ a'
]
