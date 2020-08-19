Class {
	#name : #PRRichTextComposerTest,
	#superclass : #TestCase,
	#instVars : [
		'sample'
	],
	#category : #'Pillar-ExporterRichText-Tests'
}

{ #category : #helpers }
PRRichTextComposerTest >> assertWriting: aPRDocument include: expectedAttribute [
	self assert: ((self runsOf: aPRDocument) includes: {expectedAttribute})
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> boldFormat [
	^ TextEmphasis bold
]

{ #category : #helpers }
PRRichTextComposerTest >> composer [
	^ self composerClass new
]

{ #category : #helpers }
PRRichTextComposerTest >> composerClass [
	^ PRRichTextComposer
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> externalLink: aLink [
	^ TextAction new actOnClickBlock: [WebBrowser openOn: aLink]
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> figure: aFigureLink [
	| url |
	url := aFigureLink.
	^ TextAnchor new anchoredMorph: (ZnEasy getPng: url)
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel1 [
	^ PRRichTextComposer headerLevelFont: 1
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel2 [
	^ PRRichTextComposer headerLevelFont: 2
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel3 [
	^ PRRichTextComposer headerLevelFont: 3
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel4 [
	^ PRRichTextComposer headerLevelFont: 4
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel5 [
	^ PRRichTextComposer headerLevelFont: 5
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> headerLevel6 [
	^ PRRichTextComposer headerLevelFont: 6
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> italicFormat [
	^ TextEmphasis italic
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> monospaceFormat [
	^ TextBackgroundColor color: Smalltalk ui theme settings windowColor
]

{ #category : #helpers }
PRRichTextComposerTest >> richTextFor: aPRDocument [
	^ self composer start: aPRDocument
]

{ #category : #helpers }
PRRichTextComposerTest >> runsOf: aPRDocument [

	^ (self richTextFor: aPRDocument) runs
]

{ #category : #running }
PRRichTextComposerTest >> setUp [
	super setUp.
	sample := PRDocumentSample new.
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> strikethroughtFormat [
	^ TextEmphasis struckOut
]

{ #category : #helpers }
PRRichTextComposerTest >> stringOf: aPRDocument [
	
	^ (self richTextFor: aPRDocument) string
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> subscriptFormat [
	^ TextColor red
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> superscriptFormat [
	^ TextColor blue
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> testBoldFormat [

	self assertWriting: sample bold include: self boldFormat
]

{ #category : #'tests - CodeBlock' }
PRRichTextComposerTest >> testCodeBlock [
	| runs richText |
	self skip.
	richText := self richTextFor: sample codeblock.

"
'1 + 2 
	* 3  '
"	
	runs := richText runs.	
	
	"self assert: runs first isEmpty. 
	self assert: runs second first class equals: TextIndent.
	self assert: runs second first amount equals: 1.
	self assert: runs runs equals:  #(1 3 1 15 1 1).
	"
	self assert: richText string equals: '1 + 2 
	* 3  

'
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> testExternaLink [
	| pillarLink target obj1 obj2 raised |
	self skip.
	raised := false.
	[ pillarLink := '*SitePharo>https://get.pharo.org/64/*'.
	target := 'https://get.pharo.org/64/'.

	obj1 := ((PRRichTextComposer asText: pillarLink) runs at: 1) first.
	obj2 := self externalLink: target ]
		on: Error
		do: [ raised := true ].
	self deny: raised.
	self assert: obj1 class equals: obj2 class.
	self assert: obj1 class name equals: #TextAction
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> testFigure [
	| link pillarLink obj1 obj2 raised |
	self skip.
	raised := false.
	[ pillarLink := '+Pharologo>https://files.pharo.org/media/logo/logo.png+'.
	link := 'https://files.pharo.org/media/logo/logo.png'.

	obj1 := ((PRRichTextComposer asText: pillarLink) runs at: 1) first.
	obj2 := self figure: link ]
		on: Error
		do: [ raised := true ].
	self deny: raised.
	self assert: obj1 class equals: obj2 class.
	self assert: obj1 class name equals: #TextAnchor
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel1 [
	sample := sample headerLevel1.
	self assert: sample hasChildren.
	self assertWriting: sample include: self headerLevel1
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel2 [
	self assertWriting: sample headerLevel2 include: self headerLevel2
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel3 [
	self assertWriting: sample headerLevel3 include: self headerLevel3
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel4 [
	self assertWriting: sample headerLevel4 include: self headerLevel4
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel5 [
	self assertWriting: sample headerLevel5 include: self headerLevel5
]

{ #category : #'tests - Header' }
PRRichTextComposerTest >> testHeaderLevel6 [
	self assertWriting: sample headerLevel6 include: self headerLevel6
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> testItalicFormat [
	self assertWriting: sample italic include: self italicFormat
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> testMonospaceFormat [ 
	self skip. 
	self assertWriting: sample monospace include: self monospaceFormat
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> testNestedList [
	| runs string |
	self skip.
	runs := self runsOf: sample nestedList.
	string := self stringOf: sample nestedList.
	self assert: string first equals: $1.
	self assert: runs first first class equals: TextIndent.
	self assert: runs first first amount equals: 1.
	self assert: (string at: 12) equals: $-.
	self assert: (runs at: 12) first class equals: TextIndent.
	self assert: (runs at: 12) first amount equals: 2.
	self assert: (string at: 21) equals: $-.
	self assert: (runs at: 21) first class equals: TextIndent.
	self assert: (runs at: 21) first amount equals: 2.
	self assert: (string at: 30) equals: $2.
	self assert: (runs at: 30) first class equals: TextIndent.
	self assert: (runs at: 30) first amount equals: 1.
	self assert: runs runs equals: #(3 7 1 2 6 1 2 6 1 3 7 1) asArray.
	
]

{ #category : #'tests - List' }
PRRichTextComposerTest >> testOrderedList [ 
	| runs string |
	runs := self runsOf: sample orderedList.
	string := self stringOf: sample orderedList.
	self assert: runs first first class equals: TextIndent.
	self assert: runs first first amount equals: 1.
	self assert: runs runs equals: #(3 7 1 3 7 1) asArray.
	self assert: string first equals: $1.
	self assert: (string at: 12) equals: $2.
]

{ #category : #'tests - Preformatted' }
PRRichTextComposerTest >> testPreformatted [
	| runs richText |
	richText := self richTextFor: sample preformatted.
	"
a Text for '
foo
barwith a space

'
"
	runs := richText runs.
	self assertEmpty: runs first.
	self assert: runs second first class equals: TextIndent.
	self assert: runs second first amount equals: 1.
	self assert: runs runs equals: #(1 3 1 15 1 1).
	self assert: richText string first equals: Character cr.
	self assert: richText string second equals: $f
]

{ #category : #'tests - not rendering mode' }
PRRichTextComposerTest >> testRenderCommentOf [
	
	| old |
	[ old := self composerClass notRendering.
	self composerClass notRendering: true.
	self assert: (self composerClass renderComment: 'Cocorico' of: Point) equals: 'Cocorico'.
	 ] ensure: [ self composerClass notRendering: old ]
	


]

{ #category : #'tests - not rendering mode' }
PRRichTextComposerTest >> testRenderCommentOfNotRendering [
	
	| old |
	[ old := self composerClass notRendering.
	self composerClass notRendering: true.
	self assert: (self composerClass renderComment: 'Cocorico' of: Point) equals: 'Cocorico'.
	] ensure: [ self composerClass notRendering: old ]
	


]

{ #category : #'tests - not rendering mode' }
PRRichTextComposerTest >> testRenderCommentOfRendering [
	
	| old |
	[ old := self composerClass notRendering.
	self composerClass notRendering: false.
	self deny: (self composerClass renderComment: 'Cocorico' of: Point) equals: 'Cocorico'.
	 ] ensure: [ self composerClass notRendering: old ]
	


]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> testStrikethroughFormat [ 
	"Test fails: strikethrough is recognized only if spaces surround text
	So this test fails:
		self assertWriting: factory strikethroughFormatSample include: self strikethroughtFormat.
	"
	
	self skip.
]

{ #category : #'skipped tests' }
PRRichTextComposerTest >> testSubscriptFormat [
	"Test fails: subscript isn't recognized
	
	self assertWriting: factory subscriptFormatSample include: self subscriptFormat"
	
	self skip.
	
]

{ #category : #'tests - List' }
PRRichTextComposerTest >> testUnorderedList [ 
	| runs |
	runs := self runsOf: sample unorderedList.
	self assert: runs first first class equals: TextIndent.
	self assert: runs first first amount equals: 1.
	self assert: runs runs equals: #(2 6 1 2 6 1) asArray.
]

{ #category : #'tests - Format' }
PRRichTextComposerTest >> underlineFormat [
	^ TextEmphasis underlined
]
