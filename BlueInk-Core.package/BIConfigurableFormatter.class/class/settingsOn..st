settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #blueInkFormatter)
		target: self;
		parent: #codeBrowsing;
		label: 'BlueInk Pretty Printing';
		description: 'Settings related to the formatter';
		with:
				[ 
			(aBuilder setting: #formatCommentWithStatements) label: 'Format comment with statements'.
			(aBuilder setting: #indentString) label: 'Indent string'.
			(aBuilder setting: #indentsForKeywords) label: 'Indents for keywords'.
			(aBuilder setting: #keepBlockInMessage)
				label: 'Keep block in message';
				description: (BIFormatSettingsExample >> #keepBlockInMessageExample) sourceCode.
			(aBuilder setting: #lineUpBlockBrackets)
				label: 'Line up block brackets';
				description: (BIFormatSettingsExample >> #lineUpBlockBracketsExample) sourceCode.
			(aBuilder setting: #methodSignatureOnMultipleLines)
				label: 'Method signature on multiple lines'.
			(aBuilder setting: #oneLineMessages) label: 'One line messages'.
			(aBuilder setting: #multiLineMessages) label: 'Multi line messages'.
			(aBuilder setting: #minimumNewLinesBetweenStatements)
				label: 'Minimum new lines between statements'.
			(aBuilder setting: #newLineAfterCascade)
				label: 'New line after cascade';
				description: (BIFormatSettingsExample >> #newLineAfterCascadeExample) sourceCode.
			(aBuilder setting: #newLineBeforeFirstCascade)
				label: 'New line before first cascade';
				description: (BIFormatSettingsExample >> #newLineBeforeFirstCascadeExample) sourceCode.
			(aBuilder setting: #newLineBeforeFirstKeyword)
				label: 'New line before first keyword';
				description: (BIFormatSettingsExample >> #newLineBeforeFirstKeywordExample) sourceCode.
			(aBuilder setting: #newLinesAfterMethodComment) label: 'New lines after method comment'.
			(aBuilder setting: #newLinesAfterMethodPattern) label: 'New lines after method pattern'.
			(aBuilder setting: #newLinesAfterTemporaries) label: 'New lines after temporaries'.
			(aBuilder setting: #numberOfArgumentsForMultiLine)
				label: 'Number of arguments for multi line'.
			(aBuilder setting: #periodsAsTerminators) label: 'Periods as terminators'.
			(aBuilder setting: #periodsAtEndOfBlock) label: 'Periods at end of block'.
			(aBuilder setting: #periodsAtEndOfMethod) label: 'Periods at end of method'.
			(aBuilder setting: #retainBlankLinesBeforeComments)
				label: 'Retain blank lines before comments'.
			(aBuilder setting: #retainBlankLinesBetweenStatements)
				label: 'Retain blank lines between statements'.
			(aBuilder setting: #selectorAndArgumentCombinedMaxSize)
				label: 'Selector and argument combined in method signature max size'.
			(aBuilder setting: #stringFollowingReturn) label: 'String following return'.
			(aBuilder setting: #stringInsideBlocks) label: 'String inside blocks'.
			(aBuilder setting: #stringInsideParentheses) label: 'String inside parentheses'.
			(aBuilder setting: #traditionalBinaryPrecedenceArray) label: 'Traditional binary precedence'.
			(aBuilder setting: #useBasicCommentFormat) label: 'Use basic comment format'.
			(aBuilder setting: #useTraditionalBinaryPrecedenceForParentheses)
				label: 'Use traditional binary precedence for parentheses' ]