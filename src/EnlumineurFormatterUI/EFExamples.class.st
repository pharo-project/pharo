"
This class is just holding some code to serve as pretty printer examples.
"
Class {
	#name : #EFExamples,
	#superclass : #Object,
	#classVars : [
		'settingsTable'
	],
	#category : #EnlumineurFormatterUI
}

{ #category : #accessing }
EFExamples class >> bigExamplesSettingsTable [
	^ {(#bigExample:withMethodSignatureOnMultipleLinesExample:
		->
			#(#formatCommentCloseToStatements #indentsForKeywords #lineUpBlockBrackets #methodSignatureOnMultipleLines #multiLineMessages #newLineAfterCascade #newLineBeforeFirstCascade #newLineBeforeFirstKeyword #numberOfNewLinesAfterMethodComment #numberOfNewLinesAfterTemporaries #numberOfArgumentsForMultiLine #oneLineMessages #retainBlankLinesBeforeComments #selectorAndArgumentCombinedMaxSize #useBasicCommentFormat #numberOfSpacesAfterCaretSymbolInReturn #minimumNewLinesBetweenStatements #numberOfNewLinesAfterMethodSignature #periodsAtEndOfBlock #periodsAtEndOfMethod #retainBlankLinesBetweenStatements #numberOfSpacesInsideBlock #numberOfSpacesInIndent #indentStyle)).
	(#bigExample:
		->
			#(#keepBlockInMessage #maxLineLength #numberOfSpacesInsideParentheses #numberOfSpacesInsideArray #minimumNewLinesBetweenStatements #numberOfNewLinesAfterMethodSignature #periodsAtEndOfBlock #periodsAtEndOfMethod #retainBlankLinesBetweenStatements #numberOfSpacesInsideBlock #numberOfSpacesInIndent #indentStyle))}
]

{ #category : #settings }
EFExamples class >> defaultConfiguration [
	^ 'BIEnlumineurContext {
	#formatCommentWithStatements : true,
	#indentString : ''\t'',
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
	#newLineBeforeFirstKeyword : true,
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
	#spacesAfterCaretSymbolInReturn : '' '',
	#spacesInsideParentheses : '''',
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
	],
	#numberOfSpacesInsideBlock : 1,
	#spacesInsideBlocks : '' '',
	#numberOfSpacesInsideParentheses : 0,
	#numberOfSpacesAfterCaretSymbolInReturn : 1,
	#indentStyle : #tabulation,
	#numberOfSpacesInIndent : 3
}'
]

{ #category : #settings }
EFExamples class >> empty [
	^ 'BIEnlumineurContext {
	#formatCommentWithStatements : true,
	#indentsForKeywords : 1,
	#keepBlockInMessage : true,
	#lineUpBlockBrackets : false,
	#maxLineLength : 70,
	#methodSignatureOnMultipleLines : false,
	#minimumNewLinesBetweenStatements : 1,
	#multiLineMessages : [ ],
	#newLineAfterCascade : true,
	#newLineBeforeFirstCascade : true,
	#newLineBeforeFirstKeyword : true,
	#newLinesAfterMethodComment : 2,
	#newLinesAfterMethodPattern : 1,
	#newLinesAfterTemporaries : 1,
	#numberOfArgumentsForMultiLine : 4,
	#oneLineMessages : [ ],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
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
	],
	#numberOfSpacesInsideBlock : 1,
	#numberOfSpacesInsideParentheses : 0,
	#numberOfSpacesAfterCaretSymbolInReturn : 1,
	#numberOfSpacesInIndent : 3,
	#spacesAfterCaretSymbolInReturn : '' '',
	#spacesInsideBlocks : '' '',
	#indentStyle : #tabulation,
	#indentString : ''\t'',
	#spacesInsideParentheses : '''',
	#indentExtraSpaces : '''',
	#spacesInsideArray : '' '',
	#numberOfSpacesInsideArray : 1
}'
]

{ #category : #settings }
EFExamples class >> emptyOneLineMessages [
	^ 'EFContext {
	#formatCommentWithStatements : true,
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
	#oneLineMessages : [ ],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
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
	],
	#numberOfSpacesInsideBlock : 1,
	#numberOfSpacesInsideParentheses : 1,
	#numberOfSpacesAfterCaretSymbolInReturn : 1,
	#numberOfSpacesInIndent : 3,
	#spacesAfterCaretSymbolInReturn : '' '',
	#spacesInsideBlocks : '' '',
	#indentStyle : #tabulation,
	#indentString : ''\t'',
	#spacesInsideParentheses : '' '',
	#indentExtraSpaces : '''',
	#spacesInsideArray : '' '',
	#numberOfSpacesInsideArray : 1
}'
]

{ #category : #settings }
EFExamples class >> guille [
	^ 'BIEnlumineurContext {
	#formatCommentWithStatements : true,
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
	#newLineBeforeFirstKeyword : true,
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
	],
	#numberOfSpacesInsideBlock : 1,
	#numberOfSpacesInsideParentheses : 0,
	#numberOfSpacesAfterCaretSymbolInReturn : 1,
	#numberOfSpacesInIndent : 3,
	#spacesAfterCaretSymbolInReturn : '' '',
	#spacesInsideBlocks : '' '',
	#indentStyle : #tabulation,
	#indentString : ''\t'',
	#spacesInsideParentheses : '''',
	#indentExtraSpaces : '''',
	#spacesInsideArray : '' '',
	#numberOfSpacesInsideArray : 1
}'
]

{ #category : #'class initialization' }
EFExamples class >> initializeSettingsTable [
	"self initializeSettingsTable"

	settingsTable := {(#formatCommentCloseToStatements
		-> #formatCommentWithExpressionExample).
	(#indentString -> #indentExample).
	(#indentsForKeywords -> #messageWithALotOfParameters).
	(#keepBlockInMessage -> #blockExample).
	(#lineUpBlockBrackets -> #blockExample).
	(#maxLineLength -> #maxLineLengthExample).
	(#methodSignatureOnMultipleLines -> #method:with:A:lot:of:parameters:).
	(#minimumNewLinesBetweenStatements
		-> #minimumNewLinesBetweenStatementsExample).
	#multiLineMessages.
	(#newLineAfterCascade -> #cascadeExample).
	(#newLineBeforeFirstCascade -> #cascadeExample).
	(#newLineBeforeFirstKeyword -> #messageWithALotOfParameters).
	(#numberOfNewLinesAfterMethodComment -> #newLinesExample).
	(#numberOfNewLinesAfterMethodSignature -> #newLinesExample).
	(#numberOfNewLinesAfterTemporaries -> #newLinesExample).
	(#numberOfArgumentsForMultiLine -> #messageWithALotOfParameters).
	#oneLineMessages.
	(#periodsAtEndOfBlock -> #blockExample).
	(#periodsAtEndOfMethod -> #periodsAtEndOfMethodExample).
	(#retainBlankLinesBetweenStatements -> #retainBlankLinesExample).
	(#retainBlankLinesBeforeComments -> #retainBlankLinesExample).
	(#spacesAfterCaretSymbolInReturn
		-> #spacesAfterCaretSymbolInReturnExample).
	(#spacesInsideBlocks -> #blockExample).
	(#spacesInsideParentheses -> #parenthesesExample).
	(#selectorAndArgumentCombinedMaxSize -> #messageWithALotOfParameters).
	(#useBasicCommentFormat -> #useBasicCommentFormatExample)}
]

{ #category : #settings }
EFExamples class >> multiLineMessages [
	^ 'BIEnlumineurContext {
	#formatCommentWithStatements : true,
	#indentsForKeywords : 1,
	#keepBlockInMessage : true,
	#lineUpBlockBrackets : false,
	#maxLineLength : 70,
	#methodSignatureOnMultipleLines : false,
	#minimumNewLinesBetweenStatements : 1,
	#multiLineMessages : [
		#''ifTrue:ifFalse:'',
		#''on:do:''
	],
	#newLineAfterCascade : true,
	#newLineBeforeFirstCascade : true,
	#newLineBeforeFirstKeyword : true,
	#newLinesAfterMethodComment : 2,
	#newLinesAfterMethodPattern : 1,
	#newLinesAfterTemporaries : 1,
	#numberOfArgumentsForMultiLine : 4,
	#oneLineMessages : [ ],
	#periodsAtEndOfBlock : false,
	#periodsAtEndOfMethod : false,
	#retainBlankLinesBetweenStatements : false,
	#retainBlankLinesBeforeComments : true,
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
	],
	#numberOfSpacesInsideBlock : 1,
	#numberOfSpacesInsideParentheses : 0,
	#numberOfSpacesAfterCaretSymbolInReturn : 1,
	#numberOfSpacesInIndent : 3,
	#spacesAfterCaretSymbolInReturn : '' '',
	#spacesInsideBlocks : '' '',
	#indentStyle : #tabulation,
	#indentString : ''\t'',
	#spacesInsideParentheses : '''',
	#indentExtraSpaces : '''',
	#spacesInsideArray : '' '',
	#numberOfSpacesInsideArray : 1
}'
]

{ #category : #methods }
EFExamples >> bar: arg [ 
	arg
]

{ #category : #methods }
EFExamples >> bar: arg bar2: arg1 bar3: arg2 [
	arg
]

{ #category : #methods }
EFExamples >> bar: arg bar3: arg2 [
	arg
]

{ #category : #methods }
EFExamples >> bearingToPoint2: anotherPoint [
	"Return the bearing, in degrees, from the receiver to anotherPoint."
	"questions: 
		are nested ifT indented enough?
		last - is clearly not well indented.
	"
	| deltaX deltaY |
	deltaX := anotherPoint x - self x.
	deltaY := anotherPoint y - self y.
	deltaX abs < 0.001
		ifTrue: [ ^ deltaY > 0
				ifTrue: [ 180 ]
				ifFalse: [ 0 ] ].
	^ ((deltaX >= 0
		ifTrue: [ 90 ]
		ifFalse: [ 270 ])
		- (deltaY / deltaX) arcTan negated radiansToDegrees) rounded
]

{ #category : #methods }
EFExamples >> bearingToPoint: anotherPoint [
	"Return the bearing, in degrees, from the receiver to anotherPoint."
	"questions: 
		are nested ifT indented enough?
		last - is clearly not well indented.
	"
	| deltaX deltaY |
	deltaX := anotherPoint x - self x.
	deltaY := anotherPoint y - self y.
	deltaX abs < 0.001
		ifTrue: [ ^ deltaY > 0
				ifTrue: [ 180 = 22222222222222222222 ]
				ifFalse: [ 000000000000000000000 ] ].
	^ ((deltaX >= 0
		ifTrue: [ 90 ]
		ifFalse: [ 270 ])
		- (deltaY / deltaX) arcTan negated radiansToDegrees) rounded
]

{ #category : #methods }
EFExamples >> bigArray [
^ #(#(#expr0 #expr1 #expr2 #expr3 #expr4 #expr5 #expr6 #expr7 #expr8 #expr9 #expr10 #expr11 #expr12 #expr13 #expr14 #expr15 #expr16 #expr17 #expr18 #expr19 #expr20 #expr21 #expr22 #expr23 #expr24) #(#expr25 #expr26 #expr27 #expr28 #expr29 #expr30 #expr31 #expr32 #expr33 #expr34 #expr35 #expr36 #expr37 #expr38 #expr39 #expr40 #expr41 #expr42 #expr43 #expr44 #expr45 #expr46 #expr47 #expr48 #expr49))
]

{ #category : #methods }
EFExamples >> bigExample [
1 keepBlockInMessage: [ 11111111111111111111111111111111111111111111111111111111111111111111].

1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20.

#(1 2 3 4 5 6 7 8 9 10).

1 + 2 * (3+4) + 5.
true | false = true & false.
1 + 1@1
]

{ #category : #methods }
EFExamples >> bigExample:aLoooooooooooongParameter
withMethodSignatureOnMultipleLinesExample:anotherLongParameter [
"a method comment"
| temp |

1 to: 10 do:[:i | temp doSomething; doSomethingElse].

temp ifTrue:[1] ifFalse: [ 0 ].

#(1 2 3 4 5) collect: [ :each | | t | 
	t := each factorial.
	t * 2 ] as: Set.

self method:1 with:2 aLotOf:3 parameters:4.

"     comment     "


^ temp
]

{ #category : #methods }
EFExamples >> bigMethod: argumentOne example: argumentTwo [
"   methode comment   "
|tmp|
tmp := 3.

tmp := tmp factorial.
tmp := tmp + 1.

"   Comment   "

^1
]

{ #category : #methods }
EFExamples >> blockExample [
	1 to: 4 do: [ :i | 
		Transcript
			show: i;
			cr;
			cr]
]

{ #category : #methods }
EFExamples >> bonk: glyphForm with: bonkForm [
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"

	"Uses the bonkForm to erase at every character boundary in glyphs."

	| bb offset x |
	offset := bonkForm offset x.
	bb := BitBlt toForm: glyphForm.
	bb
		sourceForm: bonkForm;
		sourceRect: bonkForm boundingBox;
		combinationRule: Form erase;
		destY: 0.
	x := self xTable.
	(x isMemberOf: SparseLargeTable)
		ifTrue: [ x base to: x size - 1 do: [ :i | 
				bb
					destX: (x at: i) + offset;
					copyBits ] ]
		ifFalse: [ 
			1 
				to: x size - 1 
				do: [ :i | 
						bb
					destX: (x at: i) + offset;
					copyBits ] ]
]

{ #category : #methods }
EFExamples >> cascadeExample [
	Transcript cr;cr;cr
]

{ #category : #methods }
EFExamples >> changeSourceSelector: oldSelector keywordsIntervals: oldIntervals arguments: oldArguments [
	"If this is the same number of arguments, we try a one to one replacement of selector parts and arguments. If this is not the c
ase try to rewrite the signature as a whole, what unfortunately drops the comments within the signature."

	| arguments |
	[ (oldIntervals size = self keywordsPositions size and: [ oldArguments size = arguments size ])
		ifTrue: [
			self selectorParts keysAndValuesDo: [ :index :part |
				self addReplacement: (RBStringReplacement 
					replaceFrom: (oldIntervals at: index) first
					to: (oldIntervals at: index) last
					with: part) ].
			oldArguments with: arguments do: [ :old :new | 
				self addReplacement: (RBStringReplacement 
					replaceFrom: old start
					to: old stop
					with: new name) ] ]
		ifFalse: [
			self addReplacement: (RBStringReplacement
				replaceFrom: oldIntervals first first
				to: (oldArguments notEmpty
					ifTrue: [ oldArguments last stop ]
					ifFalse: [ oldIntervals last last ])
				with: (String streamContents: [ :stream |
					self selectorParts keysAndValuesDo: [ :index :part |
						index = 1 ifFalse: [ stream space ].
						stream nextPutAll: part.
						index <= arguments size
							ifTrue: [ stream space; nextPutAll: (arguments at: index) name ] ] ])) ] ]
		on: Error
		do: [ :ex | ex return ]
]

{ #category : #methods }
EFExamples >> criticNameOn: aStream [
	"This behavior may be folded later by changing the name of this method or using another one."
	
	aStream 
		<< self methodClass name 
		<< '>>#' 
		<< self selector  
		<< ' (' 
		<< self methodClass instanceSide category 
		<< ')'
]

{ #category : #methods }
EFExamples >> degreesHacked [
	"Answer the angle the receiver makes with origin in degrees. right is 0; down is 90."
	
	"Questions the tan := y in the ifFalse: is not really nice it would be better to start to have it on the next line."
	
	
	| tan theta x y |
	^ x = 0
		ifTrue: [ tan := y asFloat / x asFloat ]
		ifFalse: [ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			theta := tan arcSin.
			theta := tan arcCos ]
]

{ #category : #methods }
EFExamples >> eightNeighbors [
	"why not having 
		^ {
			(self + (1 @ 0)).
			(self + (1 @ 1)).
			(self + (0 @ 1)).
			(self + (-1 @ 1)).
			(self + (-1 @ 0)).
			(self + (-1 @ -1)).
			(self + (0 @ -1)).
			(self + (1 @ -1))}
	
	"
	^ {(self + (1 @ 0)).
	(self + (1 @ 1)).
	(self + (0 @ 1)).
	(self + (-1 @ 1)).
	(self + (-1 @ 0)).
	(self + (-1 @ -1)).
	(self + (0 @ -1)).
	(self + (1 @ -1))}
]

{ #category : #methods }
EFExamples >> foo [
	| foo |
	foo 
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar2: [ 
			1 + 1.
			2 + 2 ]
		bar3: 5.
		
	foo 
		bar3: 5
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar2: [ 
			1 + 1.
			2 + 2 ].
		
	foo 
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar3: 5
		bar2: [ 
			1 + 1.
			2 + 2 ].
	foo
		bar3: 5
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar2: [ 
			1 + 1.
			2 + 2 ]
		bar3: 5
]

{ #category : #methods }
EFExamples >> foo2 [
	| foo |
	foo 
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar3: 5.
		
	foo bar3: 5 bar2: [ 
			1 + 1.
			2 + 2 ].
		
	foo 
		bar3: 5
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar3: 5.
		
	foo bar3: 5 bar3: 5 bar2: [ 
		1 + 1.
		2 + 2 ].
	
	foo 
		bar: [ 
			1 + 2.
			2 + 2 ]
		bar3: 5 
		bar3: 5
]

{ #category : #methods }
EFExamples >> formatCommentWithExpressionExample [

	1 + 1.	"We do an addition"
	2 + 2
]

{ #category : #methods }
EFExamples >> fourDirections [
	"Return vertices for a square centered at 0 asPoint with the receiver as first corner.
	Returns the four rotation of the reciever in counter clockwise order with the receiver appearing last."
	
	"Points to take into consideration: 
	When we force a space fter the ^ there is only one character left in the tab length for the with: shifting. 
	May be we should increase the tab of one to stress the difference. 
	
	"
	^ Array
		with: self leftRotated
		with: self negated
		with: self rightRotated
		with: self
]

{ #category : #methods }
EFExamples >> fourDirectionsWorkingWell [
	"Return vertices for a square centered at 0 asPoint with the receiver as first corner.
	Returns the four rotation of the reciever in counter clockwise order with the receiver appearing last."

	^ Array
		with: self leftRotated
		with: self negated
		with: self rightRotated
		with: self
]

{ #category : #methods }
EFExamples >> framePolyline: vertices on: aCanvas [ 
	"Frame the given rectangle on aCanvas"

	| prev next |
	prev := vertices first.
	2 to: vertices size
		do: 
			[:i | 
			next := vertices at: i.
			self 
				drawLineFrom: prev
				to: next
				on: aCanvas.
			prev := next]
]

{ #category : #methods }
EFExamples >> ifTrueIfFalse [

| i j|
true ifTrue: [ ^ 1 ]
	ifFalse:[ ^ 2 ].
(self isblah) ifTrue: [ 
			i := self foo. 
			j := self bar:4 ]
		ifFalse:[i := j. i].
true ifTrue: [ 
	i := j. 
	j := j + 1 ]
ifFalse:[i].
true ifTrue: [ 1]
ifFalse:[i. j].
true ifTrue: [ self foo. self bar:4]
]

{ #category : #methods }
EFExamples >> indentExample [
1 to: 4 do: [ :i | 
		i to: 4 do: [ :j |
		Transcript
			show: i;
			show: j;
			cr;
			cr] ]
]

{ #category : #methods }
EFExamples >> initializeSettingsTable [
	"self initializeSettingsTable"

	settingsTable := {(#formatCommentCloseToStatements
		-> #formatCommentWithExpressionExample).
	(#indentString -> #indentExample).
	(#indentsForKeywords -> #messageWithALotOfParameters).
	(#keepBlockInMessage -> #blockExample).
	(#lineUpBlockBrackets -> #blockExample).
	(#maxLineLength -> #maxLineLengthExample).
	(#methodSignatureOnMultipleLines -> #method:with:A:lot:of:parameters:).
	(#minimumNewLinesBetweenStatements
		-> #minimumNewLinesBetweenStatementsExample).
	#multiLineMessages.
	(#newLineAfterCascade -> #cascadeExample).
	(#newLineBeforeFirstCascade -> #cascadeExample).
	(#newLineBeforeFirstKeyword -> #messageWithALotOfParameters).
	(#numberOfNewLinesAfterMethodComment -> #newLinesExample).
	(#numberOfNewLinesAfterMethodSignature -> #newLinesExample).
	(#numberOfNewLinesAfterTemporaries -> #newLinesExample).
	(#numberOfArgumentsForMultiLine -> #messageWithALotOfParameters).
	#oneLineMessages.
	(#periodsAtEndOfBlock -> #blockExample).
	(#periodsAtEndOfMethod -> #periodsAtEndOfMethodExample).
	(#retainBlankLinesBetweenStatements -> #retainBlankLinesExample).
	(#retainBlankLinesBeforeComments -> #retainBlankLinesExample).
	(#spacesAfterCaretSymbolInReturn
		-> #spacesAfterCaretSymbolInReturnExample).
	(#spacesInsideBlocks -> #blockExample).
	(#spacesInsideParentheses -> #parenthesesExample).
	(#selectorAndArgumentCombinedMaxSize -> #messageWithALotOfParameters).
	(#useBasicCommentFormat -> #useBasicCommentFormatExample)}
]

{ #category : #methods }
EFExamples >> insideTriangle: p1 with: p2 with: p3 [
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."

	| p0 b0 b1 b2 b3 |
	p0 := self.
	b0 := (p2 x - p1 x) * (p3 y - p1 y) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero
		ifTrue: [ ^ false ].	"degenerate"
	b0 := 1.0 / b0.
	b1 := ((p2 x - p0 x) * (p3 y - p0 y)
		- ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 := ((p3 x - p0 x) * (p1 y - p0 y)
		- ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 := ((p1 x - p0 x) * (p2 y - p0 y)
		- ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	b1 < 0.0
		ifTrue: [ ^ false ].
	b2 < 0.0
		ifTrue: [ ^ false ].
	b3 < 0.0
		ifTrue: [ ^ false ].
	^ true
]

{ #category : #methods }
EFExamples >> keywords [
	"Answer the angle the receiver makes with origin in degrees. right is 0; down is 90."
	
	"Questions the tan := y in the ifFalse: is not really nice it would be better to start to have it on the next line."
	
	
	| tan theta x y |
	 x = 0
		ifTrue: [ tan := y asFloat / x asFloat ]
		ifFalse: [ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			theta := tan arcSin.
			theta := tan arcCos ].
		x = 0
			on: [ tan := y asFloat / x asFloat ]
			do: [ tan := y asFloat / x asFloat.
				theta := tan arcTan.
				theta := tan arcSin.
				theta := tan arcCos ].
		x = 0
		on: Error
		do: [ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			theta := tan arcSin.
			theta := tan arcCos ]
]

{ #category : #methods }
EFExamples >> maxLineLengthExample [
	^ 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20
]

{ #category : #methods }
EFExamples >> messageExample: a to: b [

	| i j tmp |
	tmp := 20.
	a < b
		ifTrue: [ i := a. j := b ]
		ifFalse: [ i := b.	j := a ].
	i to: j do: [:k | tmp := tmp + k].
	self method:1 with:2 aLotOf:3 parameters:4.
	^ tmp
]

{ #category : #methods }
EFExamples >> messageWithALotOfParameters [
	^ self method:1 with:2 aLotOf:3 parameters:4
]

{ #category : #methods }
EFExamples >> method: a with: b aLotOf: c parameters: d [
]

{ #category : #methods }
EFExamples >> methodSignature:aLoooooooooooongParameter
onMultipleLinesExample:anotherLongParameter [
"a method comment"
| temp |

1 to: 10 do:[:i | temp doSomething; doSomethingElse].

temp ifTrue:[1] ifFalse: [ 0 ].

self method:1 with:2 aLotOf:3 parameters:4.

"     comment     "


^ temp
]

{ #category : #methods }
EFExamples >> minimumNewLinesBetweenStatementsExample [
	1 + 1.
	2 + 2.
	3 + 3
]

{ #category : #methods }
EFExamples >> newLinesExample [
	"myComment"
	| tmp |
	tmp:= 1.
	^ tmp
]

{ #category : #methods }
EFExamples >> openSettingPreviewerWithEnlumineur [

	EFSettingPreviewer2 
		openFormaterWithAnInstanceOfFormater: EFFormatter new 
		onClass: EFExamples
]

{ #category : #methods }
EFExamples >> parenthesesExample [
	1 + ( 2 * 3 )
]

{ #category : #methods }
EFExamples >> parenthesizedExpresssionWithCascade: aBuilder [


	(aBuilder setting: #numberOfNewLinesAfterMethodSignature)
		label: 'New lines after method signature';
		description:
			'number of new lines after the method signature
Example:
myMethode: arg

	^ true
is the result for 2 new lines'.
	(aBuilder setting: #numberOfNewLinesAfterMethodSignature)
		label: 'New lines after method signature';
		description:
			'number of new lines after the method signature
Example:
myMethode: arg

	^ true
is the result for 2 new lines'
]

{ #category : #methods }
EFExamples >> periodsAtEndOfMethodExample [
	
	^ 1 factorial
]

{ #category : #methods }
EFExamples >> printOn2: stream [
	|iterations elapsedTime|
	super printOn: stream.
	stream nextPut: $(.
	self isEmpty
		ifTrue: [ 
			stream << ' empty' ]
		ifFalse: [ 
			iterations printWithCommasOn: stream.
			stream space; << ('iteration' asPluralBasedOn: iterations).
			stream << ' in '.
			elapsedTime printHumanReadableOn: stream.
			stream << '. '.
			self printFrequenceOn: stream ].
	stream nextPut: $)
]

{ #category : #methods }
EFExamples >> printOn3: s [
	[ super printOn: s ] on: Error do: [ :ex | s << '![' << ex messageText << ']!' ]
]

{ #category : #methods }
EFExamples >> printOn: aStream [
	aStream nextPutAll: 'Path'.
	self isRoot
		ifTrue: [aStream nextPutAll: ' root']
		ifFalse:
			[1 to: self size do:
				[:i |
				aStream 
					nextPutAll: ' / ''';
				 	nextPutAll: (self at: i);
					nextPut: $']]
]

{ #category : #methods }
EFExamples >> retainBlankLinesExample [
"methodComment"	

	| a b c|
	
	a:=1.


	"aComment"	

	b:=2.
	
	c:=3
]

{ #category : #methods }
EFExamples >> spacesAfterCaretSymbolInReturnExample [
	^ 1
]

{ #category : #methods }
EFExamples >> strangeFormattingWithAssignment [
	"look at this ugly parseMethod that is badly indented"
	| tree |
	tree := self
		parseMethod:
			'foo: abd bar: cde  
^ abd + cde'.
	self assert: tree conceptualSelectorSize equals: 'foo:bar:' size.
	self assert: tree conceptualArgumentSize equals: 'abdcde' size.
	self
		assert: tree conceptualSignatureSize
		equals: 'abdcde' size + 'foo:bar:' size + 3
	
]

{ #category : #methods }
EFExamples >> strangeFormattingWithAssignment2 [
	"as you see this is the length and the width of the argument that is pushing th parseMerthod on the second line"

	| tree |
	tree := self parseMethod: '1234567'.
	tree := self parseMethod: 	'a
b'.
	self
		assert: tree conceptualSignatureSize
		equals: 'abdcde' size + 'foo:bar:' size + 3
]

{ #category : #methods }
EFExamples >> sumFrom: a to: b [
	"MethodComment"
	| tmp i j |
	tmp := 0.

	(a < b)
		ifTrue: [ i := a. j := b ]
		ifFalse: [ i := b.	j := a ].
	
	"Comment"
	i to: j do: [:k | tmp := tmp + k].
	^ tmp
]

{ #category : #methods }
EFExamples >> toLongExpressionDo [
	1 to:
			([ :i | 
			    i
				    factorial;
				    + 3 ] value: 15) do: [ :j | j ]

]

{ #category : #methods }
EFExamples >> traditionalBinaryPrecedenceForParenthesesExample [

	1 + 2 * 3.
	true | false = true & true
	
]

{ #category : #methods }
EFExamples >> useBasicCommentFormatExample [
"	  aComment    "
]

{ #category : #methods }
EFExamples >> useRule41 [
	| x aBitBlt foreColorVal foreColorAlpha gammaTable gammaInverseTable |
	"x ifTrue: [ 
		aBitBlt
			copyBitsColor: foreColorVal
			alpha: foreColorAlpha
			gammaTable: gammaTable
			ungammaTable: gammaInverseTable ]	
		
"
	
	
	x
		ifTrue: [ 
			aBitBlt
				copyBitsColor: foreColorVal
				alpha: foreColorAlpha
				gammaTable: gammaTable
				ungammaTable: gammaInverseTable ]
]

{ #category : #methods }
EFExamples >> valueWithInterval: aDelay [
	"Executes the block every x milliseconds specified in arguments. Answers the process, so you can terminate it"
	
	^ [ [ self value. aDelay wait. ] repeat ] 
		forkAt: Processor userBackgroundPriority 
		named: (String streamContents: [ :s |
				s 
					<< 'every ';
					print: aDelay;
					<<' do: ';
					print: self ] )
]

{ #category : #methods }
EFExamples >> visitSequenceNode: aSequenceNode [
"how do we want to indent the cascade  
					self
					 formatCommentsFor: aSequenceNode;
					 newLine"

	self formatTemporariesFor: aSequenceNode.
	aSequenceNode comments
		ifNotEmpty: [ self
						formatCommentsFor: aSequenceNode;
						newLine ].
	self formatSequenceNodeStatementsFor: aSequenceNode
]
