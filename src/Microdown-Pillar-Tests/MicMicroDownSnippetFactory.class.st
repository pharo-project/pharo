Class {
	#name : #MicMicroDownSnippetFactory,
	#superclass : #Object,
	#instVars : [
		'parserClass'
	],
	#category : #'Microdown-Pillar-Tests'
}

{ #category : #anchor }
MicMicroDownSnippetFactory >> anchorAreTrimedSample [
	^ self parserClass anchorMarkup, '  test  '
]

{ #category : #anchor }
MicMicroDownSnippetFactory >> anchorSample [
	^ self parserClass anchorMarkup, 'test'
]

{ #category : #anchor }
MicMicroDownSnippetFactory >> anchorWithNewLineSample [
	^ self parserClass anchorMarkup, 'foo', String cr
]

{ #category : #anchor }
MicMicroDownSnippetFactory >> anchorWithSpaceInsideSample [
	^ self parserClass anchorMarkup, 'foo bar'
]

{ #category : #format }
MicMicroDownSnippetFactory >> boldFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass boldMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> boldFormatSample [
	^ self surroundWithMarkup: self parserClass boldMarkup
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> codeblock: param [ 

	^ String streamContents: [ :st | 
		st nextPutAll: self parserClass codeblockMarkup. 
		st nextPutAll: param.
		st cr.
		st nextPutAll: self parserClass codeblockMarkup. 
	]
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> codeblock: param body: body [

	^ String streamContents: [ :st | 
		st nextPutAll: self parserClass codeblockMarkup. 
		st nextPutAll: param.
		st cr.
		st nextPutAll: body.
		st cr.
		st nextPutAll: self parserClass codeblockMarkup. 
	]
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> codeblockNoParamBody: body [

	^ String streamContents: [ :st | 
		st nextPutAll: self parserClass codeblockMarkup. 
		st cr.
		st nextPutAll: body.
		st cr.
		st nextPutAll: self parserClass codeblockMarkup. 
	]
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerEmptySample [ 
	^ self parserClass headerMarkup
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel1Sample [
	^ self headerLevel: 1
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel2Sample [

	^ self headerLevel: 2
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel3Sample [

	^ self headerLevel: 3
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel4Sample [

	^ self headerLevel: 4
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel5Sample [

	^ self headerLevel: 5
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel6Sample [

	^ self headerLevel: 6
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerLevel: n [

	^ String streamContents: [ :st |
		 n timesRepeat: [ st nextPutAll: self parserClass headerMarkup ].
		 st nextPutAll: 'Foo']
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerWithFormatSample [
	^ self parserClass headerMarkup, self boldFormatSample 
]

{ #category : #headers }
MicMicroDownSnippetFactory >> headerWithNewLineSample [
	^ self parserClass headerMarkup, 'Foo', String cr
]

{ #category : #initialization }
MicMicroDownSnippetFactory >> initialize [
	
	super initialize.
	parserClass := MicroDownParser
]

{ #category : #format }
MicMicroDownSnippetFactory >> italicFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass italicMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> italicFormatSample [
	^ self surroundWithMarkup: self parserClass italicMarkup
]

{ #category : #document }
MicMicroDownSnippetFactory >> lineEndSample [

	^  ''
]

{ #category : #document }
MicMicroDownSnippetFactory >> lineEndwithNewlineSample [

	^ String lf
]

{ #category : #list }
MicMicroDownSnippetFactory >> listMultiSample [
	| unordered |
	unordered := self parserClass unorderedListMarkup.
	
	^ 	unordered, '1', String cr, 
		unordered, unordered, '2', String cr, 
		unordered, unordered, unordered, '3', String cr,
		unordered, unordered, '4', String cr,
		unordered, unordered, '5', String cr,	
		unordered, unordered, '6', String cr,
		unordered, '7', String cr,	
		unordered, '2. 8'
]

{ #category : #list }
MicMicroDownSnippetFactory >> listWithCommentSample [
	^ '1. Foo
%Comment
2. Bar'
]

{ #category : #format }
MicMicroDownSnippetFactory >> monospaceFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass monospaceMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> monospaceFormatSample [
	^ self surroundWithMarkup: self parserClass monospaceMarkup
]

{ #category : #document }
MicMicroDownSnippetFactory >> newlineCRLFSample [

	^ String crlf
]

{ #category : #document }
MicMicroDownSnippetFactory >> newlineCRSample [

	^ String cr
]

{ #category : #document }
MicMicroDownSnippetFactory >> newlineLFSample [

	^ String lf
]

{ #category : #document }
MicMicroDownSnippetFactory >> newlineSample [

	^ Smalltalk os lineEnding
]

{ #category : #list }
MicMicroDownSnippetFactory >> orderedListEmptySample [
	^ '1. '
]

{ #category : #list }
MicMicroDownSnippetFactory >> orderedListSample [
	^ '1. Foo'
]

{ #category : #list }
MicMicroDownSnippetFactory >> orderedListWithTwoItemsSample [
	
	^ 
'1. first
2. second
'
]

{ #category : #utilities }
MicMicroDownSnippetFactory >> parserClass [

	^ MicroDownParser
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterEscapeSample [
	^ 'value\==foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterSample [
	^ 'value=foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterTrueByDefaultSample [
	^ self codeblock: 'foo' body: 'bar'
	
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterValueSample [
	^ '=foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterWithFloatSample [
	^ self codeblock: 'ratio=0.8' body: 'bar'
	
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterWithLiteralArraySample [
	^ self codeblock: 'foo=#(1 2 true)'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterWithNoValuesSample [
	^ self codeblock: 'foo=' body: 'bar'

]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterWithSpacesBeforeAndAfterSample [
	^ ' foo = true | bar = 2 
'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParameterWithoutValueSample [
	^ 'foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParametersEscapeSample [
	^ 'value=foo\||bar
'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParametersMultipleSample [
	^ 'value=foo|bar
'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptParametersSample [
	^ 'value=foo
'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptSample [
	^ self codeblockNoParamBody: 'foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptTwoParametersNoValueSample [
	^ 'foo|bar
'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptWithLanguageSample [
	^ self codeblock: 'language=smalltalk' body: 'foo'
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptWithNewLineSample [
	^ (self codeblockNoParamBody: 'foo'), String cr
]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptWithSpaceAtTheEndSample [
		
	 ^ (self codeblockNoParamBody: 'foo'), '  '.


]

{ #category : #'code block' }
MicMicroDownSnippetFactory >> scriptWithSpacedParametersSample [
	^ self codeblock:   ' foo = true | bar = 2 '
body: 'bla'
]

{ #category : #format }
MicMicroDownSnippetFactory >> strikethroughFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass strikethroughMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> strikethroughFormatSample [
	^ self surroundWithMarkup: self parserClass strikethroughMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> subscriptFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass subscriptMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> subscriptFormatSample [
	^ self surroundWithMarkup: self parserClass subscriptMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> superscriptFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass superscriptMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> superscriptFormatSample [
	^ self surroundWithMarkup: self parserClass superscriptMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> surround: aWord withMarkup: aMarkup [
	^ (aMarkup, aWord, aMarkup) asString
]

{ #category : #format }
MicMicroDownSnippetFactory >> surroundWithMarkup: aMarkup [
	^ self surround: 'Foo' withMarkup: aMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> underlineFormatEmptySample [
	^ self surround: '' withMarkup: self parserClass underlineMarkup
]

{ #category : #format }
MicMicroDownSnippetFactory >> underlineFormatSample [
	^ self surroundWithMarkup: self parserClass underlineMarkup
]

{ #category : #list }
MicMicroDownSnippetFactory >> unorderedListEmptySample [
	^ self parserClass unorderedListMarkup
]

{ #category : #list }
MicMicroDownSnippetFactory >> unorderedListSample [
	^ self parserClass unorderedListMarkup, 'Foo'
]

{ #category : #list }
MicMicroDownSnippetFactory >> unorderedListWithSubListSample [
	| listMarkup |
	listMarkup := self parserClass unorderedListMarkup.
	^ listMarkup, 'Foo', String cr, listMarkup, listMarkup, 'Bar'
]

{ #category : #list }
MicMicroDownSnippetFactory >> unorderedListWithTwoItemsSample [
	| listMarkup |
	listMarkup := self parserClass unorderedListMarkup.
	^ listMarkup, 'Foo', String cr, listMarkup, 'Bar'
]
