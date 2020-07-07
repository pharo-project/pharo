"
I'm a textual builder of microdown. 
Using my API clients can generate text in Microdown format.

I should replace MicBuilder and probably be moved to Microdown project.


#### Known limits 
Nested lists are probably not well managed.
"
Class {
	#name : #MicMicrodownTextualBuilder,
	#superclass : #MicAbstractMicrodownTextualBuilder,
	#instVars : [
		'prefixStack',
		'i'
	],
	#category : #'Microdown-Parser'
}

{ #category : #'element - annotated' }
MicMicrodownTextualBuilder >> annotated: annotation paragraph:  aText [
	"!!Important 
	
	attention there is not space between the !! and the label (annotation in pillar) or we should improve the microdown parser"
	self flag: #fixme. "Should have the same API than paragraph for the paragraph"
	self
		raw: parserClass annotatedParagraphMarkup;
		raw: annotation; 
		raw: String space;
		raw: aText.
	self newLine

]

{ #category : #'element - annotated' }
MicMicrodownTextualBuilder >> annotatedAnnotation: annotation [
	"I'm made to be followed by a paragraph. I manage the space between the annotation/label and the paragraph."
	
	"!!Important there is not space between the !! and the label (annotation in pillar) or we should improve the microdown parser"
	self
		raw: parserClass annotatedParagraphMarkup;
		raw: annotation;
		raw: String space
	

]

{ #category : #'element - code block' }
MicMicrodownTextualBuilder >> codeblock: aString [ 
	self flag: #fixme. 
	"missing first line stuff"
	self raw: parserClass codeblockMarkup.
	self newLine.
	self raw: aString.
	self newLine.
	self raw: parserClass codeblockMarkup.
]

{ #category : #'element - code block' }
MicMicrodownTextualBuilder >> codeblockTag: aTag withBody: aString [
	self flag: #fixme. 
	"missing first line stuff and we need a block for value of keys
	because we can write ```language=pharo&caption=The **bold** and `mono`
	...
	"

	self raw: parserClass codeblockMarkup.
	self raw: aTag.	
	self newLine.
	self raw: aString.
	self newLine.
	self raw: parserClass codeblockMarkup.
]

{ #category : #'element - header' }
MicMicrodownTextualBuilder >> comment: aText [
	self 
		withLinesIn: aText 
		do: [ :line | self writeText: line beginsWith: (parserClass commentedLineMarkup), String space ] 		separatedBy: [ self newLine ]
	
]

{ #category : #'element - header' }
MicMicrodownTextualBuilder >> duringHeaderLevel: anInteger do: aBlockClosure [ 

	self
		raw: (parserClass headerMarkup repeat: anInteger);
		raw: String space.
	aBlockClosure value
]

{ #category : #'element - references' }
MicMicrodownTextualBuilder >> externalLink: aURL withDescription: aDescription [
	self 
		writeText: aDescription 
		surroundBy: parserClass linkNameOpenerMarkup 
		and: parserClass linkNameCloserMarkup.
	self 
		writeText: aURL 
		surroundBy: parserClass urlOpenerMarkup 
		and: parserClass urlCloserMarkup
]

{ #category : #'element - figure' }
MicMicrodownTextualBuilder >> figure: aFigureUrl withDescription: aDescription [
	self flag: #fixme.
	"missing parameter and need a builder like in header."
	self 
		writeText: aDescription 
		surroundBy: parserClass figureOpenerMarkup 
		and: parserClass linkNameCloserMarkup.
	self 
		writeText: aFigureUrl 
		surroundBy: parserClass urlOpenerMarkup 
		and: parserClass urlCloserMarkup
]

{ #category : #'element - figure' }
MicMicrodownTextualBuilder >> figure: aFigureUrl withDescription: aDescription withParameters: aZnDictionary [
	| stringToAdd |
	aZnDictionary 
		ifEmpty: [ self figure: aFigureUrl withDescription: aDescription ] 
		ifNotEmpty: [
			self flag: #fixme.
			"we should avoid to concatenate all such string and use the builder internals."
			stringToAdd := String empty.
			aZnDictionary keysAndValuesDo: [ :key :value | stringToAdd := stringToAdd, parserClass argumentListDelimiter, key asString, parserClass argumentListEqualsDelimiter, value asString ].
		stringToAdd := parserClass argumentListStartDelimiter, stringToAdd allButFirst.
		self figure: aFigureUrl, stringToAdd withDescription: aDescription ]
]

{ #category : #'element - header' }
MicMicrodownTextualBuilder >> header: aBloc withLevel: anInteger [
	self newLine.
	self raw: (parserClass headerMarkup repeat: anInteger).
   self raw: String space.
   aBloc value: self.
	self newLine
]

{ #category : #'element - header' }
MicMicrodownTextualBuilder >> horizontalLine [
	self raw: parserClass horizontalLineMarkup.
   self newLine

]

{ #category : #initialization }
MicMicrodownTextualBuilder >> initialize [
	super initialize.
	prefixStack := OrderedCollection new.
	
]

{ #category : #'element - references' }
MicMicrodownTextualBuilder >> internalLink: aLabel [
	self 
		writeText: aLabel 
		surroundBy: parserClass anchorReferenceOpenerMarkup 
		and: parserClass anchorReferenceCloserMarkup.
	
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> item: aBlock [

	aBlock value.
	self newLine.
]

{ #category : #'ugly line handling' }
MicMicrodownTextualBuilder >> lines: aString [
	"Output aString and take care of line ending within aString."
	| str |
	str := aString readStream.
	[ str atEnd ] whileFalse: [ | line |
		line := str nextLine.
		str atEnd
			ifTrue: [ self nextPutAll: line ]
			ifFalse: [ self line: line ]
	 ]
]

{ #category : #'ugly line handling' }
MicMicrodownTextualBuilder >> nextPutAllLines: aString [
	self 
		withLinesIn: aString 
		do: [ :line | self nextPutAll: line ] 
		separatedBy: [ self newLine ]
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> orderedItem: aBlock [
	self writeOrderedListIndex: 1.
	aBlock value.
	self newLine.
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> orderedItem: aBlock startingAt: aNumber [

	self writeOrderedListIndex: aNumber.
	aBlock value.
	self newLine.
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> orderedListDuring: aBlockClosure [
	"For list we do not emit empty line at the end because we do not want to force the creation of a paragraph 
	in the middle of nested list. And from a builder point of view this is difficult to know it."
	
	
	aBlockClosure value.
	"for now we do not care to generate 
	1. 
	2.
	
	but 
	1. 
	1.
	because this is the same"
		
]

{ #category : #'element - paragraph' }
MicMicrodownTextualBuilder >> paragraph: aBlock [

	aBlock value. 
	self newLine
]

{ #category : #'private utils' }
MicMicrodownTextualBuilder >> popPrefix [
	
	prefixStack removeLast
]

{ #category : #'private utils' }
MicMicrodownTextualBuilder >> pushPrefix: aString [ 
	
	prefixStack addLast: aString
]

{ #category : #'element - quote block' }
MicMicrodownTextualBuilder >> quoteBlock: aText [

	self writeText: aText beginsWith: parserClass preformattedMarkup
]

{ #category : #'writing low-level' }
MicMicrodownTextualBuilder >> raw: aString [
	"We have in the prefix potentially many nesting level '> ', '```' and for each new line 
	we want to make sure that prefixes are written prior to aString.
	
	Now lastIsNewLine indicates that we should only do that for new line since 
	raw can be invoked for multiple reasons. Now the logic of writeIndentedCodeBlockDuring: only works if 
	this raw is invoked. 
	"
	lastIsNewLine ifTrue: [ 
		prefixStack do: [ :each |
			super raw: each ].
	].
	super raw: aString.
	lastIsNewLine := false
]

{ #category : #'ugly line handling' }
MicMicrodownTextualBuilder >> rawLines: aString [
	"Output aString and take care of line ending within aString."
	self withLinesIn: aString do: [ :line | self raw: line ] separatedBy: [ self newLine ]
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> unorderedListDuring: aBlockClosure [
	"For list we do not emit empty line at the end because we do not want to force the creation of a paragraph 
	in the middle of nested list. And from a builder point of view this is difficult to know it."
	self
		writePrefix: parserClass unorderedListMarkup
		forEachLineDuring: aBlockClosure.
]

{ #category : #'ugly line handling' }
MicMicrodownTextualBuilder >> withLinesIn: aString do: aBlock separatedBy: anotherBlock [
	"this method shows that the PRCommentedLine class is weak because it should encapsulate the way it internally represents lines. Now this is exposed in clients."
	| str |
	str := aString readStream.
	[ str atEnd ]
		whileFalse: [ 
			| line |
			line := str nextLine.
			aBlock value: line.
			str atEnd
				ifFalse: anotherBlock ]
]

{ #category : #'writing during' }
MicMicrodownTextualBuilder >> writeIndentedCodeBlockDuring: aBlockClosure [ 
	"The logic of writeIndentedCodeBlockDuring: only works if the raw: method managing the prefixes
is invoked. Therefore somehow the blockclosure execution should invoke it.
It means that within the context of a visitor the visitText: method should do it."
	
	self
		writePrefix: parserClass preformattedMarkup
		forEachLineDuring: aBlockClosure.
]

{ #category : #'element - list' }
MicMicrodownTextualBuilder >> writeOrderedListIndex: anInteger [
	self raw: anInteger asString, parserClass orderedListSemiMarkup
]

{ #category : #'writing during' }
MicMicrodownTextualBuilder >> writePrefix: aString forEachLineDuring: aBlockClosure [ 
	"The logic of writePrefix:forEachLineDuring: only works if the raw: method managing the prefixes
is invoked. Therefore somehow the blockclosure execution should invoke it.
It means that within the context of a visitor the visitText: method should do it."
	self pushPrefix: aString.
	aBlockClosure ensure: [ self popPrefix ].
]

{ #category : #'writing during' }
MicMicrodownTextualBuilder >> writeQuoteCodeBlockDuring: aBlockClosure [ 
	"The logic of writePrefix:forEachLineDuring: only works if the raw: method managing the prefixes
is invoked. Therefore somehow the blockclosure execution should invoke it.
It means that within the context of a visitor the visitText: method should do it."
	self
		writePrefix: parserClass preformattedMarkup, String space
		forEachLineDuring: aBlockClosure
]
