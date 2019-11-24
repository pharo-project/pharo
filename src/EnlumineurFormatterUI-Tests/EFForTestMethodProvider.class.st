"
I'm using to test the BISettingPreviewer
"
Class {
	#name : #EFForTestMethodProvider,
	#superclass : #Object,
	#category : #'EnlumineurFormatterUI-Tests'
}

{ #category : #utils }
EFForTestMethodProvider class >> contentRemovable [
	^ 'BIPrettyPrinterContext {
	#formatCommentWithStatements : true,
	#indentString : ''-'',
	#indentsForKeywords : 1,
	#keepBlockInMessage : true,
	#lineUpBlockBrackets : false,
	#maxLineLength : 70,
	#methodSignatureOnMultipleLines : false,
	#minimumNewLinesBetweenStatements : 1,
	#multiLineMessages : [
		#''ifTrue:ifFalse:'',
		#''ifFalse:ifTrue:'',
		#''ifTrue:'',
		#''ifFalse:'',
		#''on:do:'',
		#''ensure:'',
		#''ifCurtailed:''
	],
	#newLineAfterCascade : true,
	#newLineBeforeFirstCascade : true,
	#newLineBeforeFirstKeyword : false,
	#newLinesAfterMethodComment : 2,
	#newLinesAfterMethodPattern : 1,
	#newLinesAfterTemporaries : 1,
	#numberOfArgumentsForMultiLine : 4,
	#oneLineMessages : [
		#''to:'',
		#''to:do:'',
		#''to:by:'',
		#''to:by:do:''
	],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
	#stringFollowingReturn : '' '',
	#stringInsideBlocks : '' '',
	#stringInsideParentheses : '''',
	#useTraditionalBinaryPrecedenceForParentheses : true,
	#selectorAndArgumentCombinedMaxSize : 40,
	#useBasicCommentFormat : true,
	#traditionalBinaryPrecedenceArray : [
		[
			Character [ ''|'' ],
			Character [ ''&'' ],
			Character [ ''?'' ]
		],
		[
			Character [ ''='' ],
			Character [ ''~'' ],
			Character [ ''<'' ],
			Character [ ''>'' ]
		],
		[
			Character [ ''-'' ],
			Character [ ''+'' ]
		],
		[
			Character [ ''*'' ],
			Character [ ''/'' ],
			Character [ ''%'' ],
			Character [ ''\\'' ]
		],
		[
			Character [ ''@'' ]
		]
	]
}'
]

{ #category : #settings }
EFForTestMethodProvider class >> testConfigIdentStringTabulation [
	^ 'BIPrettyPrinterContext {
	#formatCommentWithStatements : false,
	#indentString : ''\t'',
	#indentsForKeywords : 1,
	#keepBlockInMessage : false,
	#lineUpBlockBrackets : false,
	#maxLineLength : 70,
	#methodSignatureOnMultipleLines : false,
	#minimumNewLinesBetweenStatements : 1,
	#multiLineMessages : [
		#''ifTrue:ifFalse:'',
		#''ifFalse:ifTrue:'',
		#''ifTrue:'',
		#''ifFalse:'',
		#''on:do:'',
		#''ensure:'',
		#''ifCurtailed:''
	],
	#newLineAfterCascade : false,
	#newLineBeforeFirstCascade : true,
	#newLineBeforeFirstKeyword : false,
	#newLinesAfterMethodComment : 2,
	#newLinesAfterMethodPattern : 1,
	#newLinesAfterTemporaries : 1,
	#numberOfArgumentsForMultiLine : 4,
	#oneLineMessages : [
		#''to:'',
		#''to:do:'',
		#''to:by:'',
		#''to:by:do:''
	],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
	#stringFollowingReturn : '' '',
	#stringInsideBlocks : '' '',
	#stringInsideParentheses : '''',
	#useTraditionalBinaryPrecedenceForParentheses : true,
	#selectorAndArgumentCombinedMaxSize : 40,
	#useBasicCommentFormat : true,
	#traditionalBinaryPrecedenceArray : [
		[
			Character [ ''|'' ],
			Character [ ''&'' ],
			Character [ ''?'' ]
		],
		[
			Character [ ''='' ],
			Character [ ''~'' ],
			Character [ ''<'' ],
			Character [ ''>'' ]
		],
		[
			Character [ ''-'' ],
			Character [ ''+'' ]
		],
		[
			Character [ ''*'' ],
			Character [ ''/'' ],
			Character [ ''%'' ],
			Character [ ''\\'' ]
		],
		[
			Character [ ''@'' ]
		]
	]
}'
]

{ #category : #settings }
EFForTestMethodProvider class >> textConfigIdentStringFourSpace [
	^ 'BIPrettyPrinterContext {
	#formatCommentWithStatements : true,
	#indentString : ''    '',
	#indentsForKeywords : 1,
	#keepBlockInMessage : true,
	#lineUpBlockBrackets : false,
	#maxLineLength : 70,
	#methodSignatureOnMultipleLines : false,
	#minimumNewLinesBetweenStatements : 1,
	#multiLineMessages : [
		#''ifTrue:ifFalse:'',
		#''ifFalse:ifTrue:'',
		#''ifTrue:'',
		#''ifFalse:'',
		#''on:do:'',
		#''ensure:'',
		#''ifCurtailed:''
	],
	#newLineAfterCascade : true,
	#newLineBeforeFirstCascade : true,
	#newLineBeforeFirstKeyword : false,
	#newLinesAfterMethodComment : 2,
	#newLinesAfterMethodPattern : 1,
	#newLinesAfterTemporaries : 1,
	#numberOfArgumentsForMultiLine : 4,
	#oneLineMessages : [
		#''to:'',
		#''to:do:'',
		#''to:by:'',
		#''to:by:do:''
	],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
	#stringFollowingReturn : '' '',
	#stringInsideBlocks : '' '',
	#stringInsideParentheses : '''',
	#useTraditionalBinaryPrecedenceForParentheses : true,
	#selectorAndArgumentCombinedMaxSize : 40,
	#useBasicCommentFormat : true,
	#traditionalBinaryPrecedenceArray : [
		[
			Character [ ''|'' ],
			Character [ ''&'' ],
			Character [ ''?'' ]
		],
		[
			Character [ ''='' ],
			Character [ ''~'' ],
			Character [ ''<'' ],
			Character [ ''>'' ]
		],
		[
			Character [ ''-'' ],
			Character [ ''+'' ]
		],
		[
			Character [ ''*'' ],
			Character [ ''/'' ],
			Character [ ''%'' ],
			Character [ ''\\'' ]
		],
		[
			Character [ ''@'' ]
		]
	]
}'
]

{ #category : #methods }
EFForTestMethodProvider >> useForTestWillBeModified [
^ 'what i have in my hat ? A Banana'
]

{ #category : #methods }
EFForTestMethodProvider >> useForTestWillBeRemove [
^ 'klmklm' 
]
