"
# Microdown Parser and Elements

I'm a parser for Microdown, implemented by S. Ducasse, L. Dargaud and G. Polito (2020). 
It is based on K. Osterbye's work on GitHubMarkdown. 

Microdown covers Markdown basis syntax, with great Pillar's features. It's more extensible too.

## MicroDown Syntax in a Nutshell!

**Headers** (up to 6 levels) >>>> `# Header level 1`

**Unordered list**
```
- item a
  on another line
- item b
   - item b1
   - item b2
- item c
```

produces

- item a
  on another line
- item b
   - item b1
   - item b2
- item c

**Ordered list**
```
1. item one
   1. subitem
2. item two
```
produces 

1. item one
   1. subitem
2. item two


**Codeblock with arguments**
```
 ```language=Pharo&caption=Beautiful&label=Fig1
 some piece of code
 ```
```

produces 

```language=Pharo&caption=Beautiful&label=Fig1
some piece of code
```

**Anchor and its reference** >>>> `\@anchor` and its reference in text with `\*@anchor@*`

**Math environment** (rendered with LaTex in Pharo)
- inline: $\cos(\theta_i+C_3)$ >>>> `\$\cos(\theta_i+C_3)$`
- block: 
```
 $
 \cos(\theta_i+C_3)
 $
```
**Annotated paragraph**
``` 
 !!note Then you can write your note
 on several lines, like a normal paragraph
```
**Quote** to indent some lines
```
> This is
> A nice quote
```
**Environment** (called Annotation for inline)
- inline: `\<?keyword|key1=value1&key2=value2?>`
- block: 
```
 <?keyword|key1=value1&key2=value2
 type your content
 ?>
```
**Metadata** as a block
```
 { 
 type your metadata
 }
```

**Table** **@tocomment**

**Horizontal line** >>> `***` alone on a line will define a separator in your text **@todo**

**Comments** don't appear after parsing >>> `% A commented line`

**Inline formats**
- **bold** >>>> `**bold**`
- _italic_ >>>> `\_italic_`
- `monospace` aka inline code >>>> `\`monospace\``

_Note: inline escape character is back slash: \\ ._
_Note: text in monospace is analyzed for you to generate hypertext Pharo code objects! Isn't it so cool to write a great doc? It is! Click on them!_
- `Point` for classes
- `Point class` for class side
- `Point>>#setX:setY:` for methods
- `#'Microdown-Tests'` for packages

**Raw** for your other code (inline) >>>> `\{{ some code }}`

**Link** >>>> `\[link's name](url|key1=value1&key2=value2)`

**Figure** >>>> `\!\[figure's name](url|key1=value1&key2=value2)`

`![Pharo logo](https://files.pharo.org/media/logo/logo.png)`
produces

![Pharo logo](https://files.pharo.org/media/logo/logo.png)


## Implementation

I follow the design mentioned in [https://github.github.com/gfm](https://github.github.com/gfm), in particular the parsing strategy in appendix A.

In short, the strategy is that at any point in time, we might have a number of children of the root which are ""open"". The deepest in open in the tree is called ""current"". All the parents of current are open. 

When a new line is read we do the following:

1. Check if the new line can be consumed by current.
   - as part of this, a child of current can be made which can consume the new line. for example when consuming \`\`\` the root block node will create, a new code block that will become current and consume the body of the \`\`\` element then close. 
2. If current cannot consume the new line, we close current, move current to its parent, and repeat 1.
3. The root node can consume anything, for instance by making new nodes for storing the new line.
4. The root node is not closed until input is exhausted.

I do not accept lazy definition. I do not accept three different ways to do the same. Except for bulleted list where `*` and `-` are accepted. 

The spec says:
```
-> document
  -> block_quote
       paragraph
         ""Lorem ipsum dolor\nsit amet.""
    -> list (type=bullet tight=true bullet_char=-)
         list_item
           paragraph
             ""Qui *quodsi iracundia*""
      -> list_item
        -> paragraph
             ""aliquando id""
```
Now the implementation for now does not create a paragraph in the list_item element. 

"
Class {
	#name : #MicroDownParser,
	#superclass : #Object,
	#instVars : [
		'current',
		'root',
		'dispatchTable'
	],
	#classInstVars : [
		'inlineMarkups'
	],
	#category : #'Microdown-Parser'
}

{ #category : #private }
MicroDownParser class >> allBlockNodeClasses [
	^ MicAbstractBlock allSubclasses
	
]

{ #category : #'block markups' }
MicroDownParser class >> anchorMarkup [
	^ '@'
]

{ #category : #'inline block markups' }
MicroDownParser class >> anchorReferenceCloserMarkup [
	^ self inlineMarkups at: #anchorReferenceCloser
]

{ #category : #'inline block markups' }
MicroDownParser class >> anchorReferenceOpenerMarkup [
	^ self inlineMarkups at: #anchorReferenceOpener
]

{ #category : #'block markups' }
MicroDownParser class >> annotatedParagraphMarkup [
	^ '!!'
]

{ #category : #'markup utils' }
MicroDownParser class >> argumentListDelimiter [
	
	^ '&'
]

{ #category : #'markup utils' }
MicroDownParser class >> argumentListEqualsDelimiter [
	
	^ '='
]

{ #category : #'markup utils' }
MicroDownParser class >> argumentListOfFigureStartDelimiter [
	
	^ '?'
]

{ #category : #'markup utils' }
MicroDownParser class >> argumentListStartDelimiter [
	
	^ '|'
]

{ #category : #'inline block markups' }
MicroDownParser class >> boldMarkup [
	^ self inlineMarkups at: #bold
]

{ #category : #private }
MicroDownParser class >> builder [
	^ MicMicrodownTextualBuilder new
]

{ #category : #'block markups' }
MicroDownParser class >> codeblockMarkup [

	^ '```'
]

{ #category : #'block markups' }
MicroDownParser class >> commentedLineMarkup [

	^ '%'
]

{ #category : #'block markups' }
MicroDownParser class >> environmentClosingBlockMarkup [
	^ '?>'
]

{ #category : #'block markups' }
MicroDownParser class >> environmentOpeningBlockMarkup [
	^ '<?'
]

{ #category : #examples }
MicroDownParser class >> example [
	<sampleInstance>
	^ self parse: self comment
]

{ #category : #'inline block markups' }
MicroDownParser class >> figureOpenerMarkup [
	^ self inlineMarkups at: #figureOpener
]

{ #category : #'block markups' }
MicroDownParser class >> headerMarkup [

	^ '#'
]

{ #category : #'block markups' }
MicroDownParser class >> horizontalLineMarkup [
	^ '***'
]

{ #category : #'inline block markups' }
MicroDownParser class >> inlineMarkup: aType [
	^ self inlineMarkups at: aType asSymbol
]

{ #category : #'markup utils' }
MicroDownParser class >> inlineMarkups [
	^ inlineMarkups ifNil: [ 
		inlineMarkups := Dictionary new.
		MicAbstractDelimiter subclasses 
			do: [ :subclass | 
			| role |
			self flag: #fixMe. 
			"why not pushing this logic in the class themselves? Because a delimiter knows it."
			"for example for there is also a problem with escape which is neither a closer and opener"
			role := subclass isOpener 
				ifTrue: [ subclass isCloser 
								ifTrue: [ '' ] 
								ifFalse: [ 'Opener' ] ] 
				ifFalse: [ 'Closer' ].			
			inlineMarkups
				at: subclass type asString, role
				put: subclass markup ].
			^ inlineMarkups ]
]

{ #category : #'inline block markups' }
MicroDownParser class >> italicMarkup [
	^ self inlineMarkups at: #italic
]

{ #category : #'inline block markups' }
MicroDownParser class >> linkNameCloserMarkup [
	^ self inlineMarkups at: #linkNameCloser
]

{ #category : #'inline block markups' }
MicroDownParser class >> linkNameOpenerMarkup [
	^ self inlineMarkups at: #linkNameOpener
]

{ #category : #'block markups' }
MicroDownParser class >> mathClosingBlockMarkup [
	
	^ '&'
]

{ #category : #'inline block markups' }
MicroDownParser class >> mathMarkup [
	^ self inlineMarkups at: #math
]

{ #category : #'block markups' }
MicroDownParser class >> mathOpeningBlockMarkup [

	^ '&'
]

{ #category : #'block markups' }
MicroDownParser class >> metaDataClosingBlockMarkup [
	^ '}'
]

{ #category : #'block markups' }
MicroDownParser class >> metaDataOpeningBlockMarkup [
	^ '{'
]

{ #category : #'inline block markups' }
MicroDownParser class >> monospaceMarkup [
	^ self inlineMarkups at: #monospace
]

{ #category : #'block markups' }
MicroDownParser class >> orderedListSemiMarkup [

	^ '. '
]

{ #category : #'instance creation' }
MicroDownParser class >> parse: aString [

	^ self new parse: aString
]

{ #category : #'block markups' }
MicroDownParser class >> preformattedMarkup [

	^ '> '
]

{ #category : #'markup utils' }
MicroDownParser class >> resetInlineMarkups [
	"self resetInlineMarkups"
	inlineMarkups := nil
]

{ #category : #'inline block markups' }
MicroDownParser class >> strikeMarkup [
	^ self inlineMarkups at: #strike
]

{ #category : #'block markups' }
MicroDownParser class >> tableCellMarkup [

	^ '|'
]

{ #category : #'block markups' }
MicroDownParser class >> unorderedListMarkup [

	^ '- '
]

{ #category : #'block markups' }
MicroDownParser class >> unorderedListMarkups [

	^ { self unorderedListPlusMarkup . self unorderedListMarkup . self unorderedListStarMarkup}
]

{ #category : #'block markups' }
MicroDownParser class >> unorderedListPlusMarkup [

	^ '+ '
]

{ #category : #'block markups' }
MicroDownParser class >> unorderedListStarMarkup [

	^ '* '
]

{ #category : #'inline block markups' }
MicroDownParser class >> urlCloserMarkup [
	^ self inlineMarkups at: #urlCloser
]

{ #category : #'inline block markups' }
MicroDownParser class >> urlOpenerMarkup [
	^ self inlineMarkups at: #urlOpener
]

{ #category : #markups }
MicroDownParser >> anchorMarkup [
	^ self class anchorMarkup
]

{ #category : #markups }
MicroDownParser >> annotatedParagraphMarkup [
	^ self class annotatedParagraphMarkup
]

{ #category : #markups }
MicroDownParser >> argumentListOfFigureStartDelimiter [
	
	^ self class argumentListOfFigureStartDelimiter
]

{ #category : #markups }
MicroDownParser >> argumentListStartDelimiter [
	
	^ self class argumentListStartDelimiter
]

{ #category : #'node creation' }
MicroDownParser >> blockStarterClassFrom: line [
	"return the class of a block which can start with line, or nil if none"

	line ifEmpty: [ ^ nil ].
	(self matchOrdered: line)
		ifTrue: [ ^ MicOrderedListBlock ]
		ifFalse: [ 
			"the max length of a markup is 3 even if header can be #####"
			1 to: 3 do: [:i |
				line size < i
					ifTrue: [ ^ MicParagraphBlock ].
  				dispatchTable
    		 		 at: (line first: i)
      				ifPresent: [ :blockClass | ^ blockClass ] ]].
	line 
		ifNotEmpty: [ ^ MicParagraphBlock ] 
	
]

{ #category : #'node creation' }
MicroDownParser >> blockStarterClassFromOld: line [
	"return the class of a block which can start with line, or nil if none"

	(line beginsWith: self headerMarkup)
		ifTrue: [ ^ MicHeaderBlock ].
	(line beginsWith: self codeblockMarkup)
		ifTrue: [ ^ MicCodeBlock ].
	(self matchUnordered: line)
		ifTrue: [ ^ MicUnorderedListBlock ].
			"this one could be split if we move to a dictionary based structure"
	(self matchOrdered: line)
		ifTrue: [ ^ MicOrderedListBlock ].
				"number followed by . or ). We could treat this one separated and fall back on a dictionary By moving this into the extension for example."
	(line beginsWith: self annotatedParagraphMarkup)
		ifTrue: [ ^ MicAnnotatedBlock ].
	(line beginsWith: self anchorMarkup)
		ifTrue: [ ^ MicAnchorBlock ].
	(line beginsWith: self commentedLineMarkup)
		ifTrue: [ ^ MicCommentBlock ].
	(line beginsWith: self mathOpeningBlockMarkup)
		ifTrue: [ ^ MicMathBlock ].
	(line beginsWith: self environmentOpeningBlockMarkup)
		ifTrue: [ ^ MicEnvironmentBlock ].
	(line beginsWith: self horizontalLineMarkup)
		ifTrue: [ ^ MicHorizontalLineBlock ].
	(line beginsWith: self metaDataOpeningBlockMarkup)
		ifTrue: [ ^ MicMetaDataBlock ].
	(line beginsWith: self preformattedMarkup)
		ifTrue: [ ^ MicQuoteBlock ].	"or '> '??"
	(line beginsWith: self tableCellMarkup)
		ifTrue: [ ^ MicTableBlock ].
	line ifNotEmpty: [ ^ MicParagraphBlock ] ifEmpty: [ ^ nil ]
]

{ #category : #markups }
MicroDownParser >> boldMarkup [

	^ self class boldMarkup
]

{ #category : #accessing }
MicroDownParser >> builder [
	"return a little helper to build microdown correct expression"
	
	^ self class builder
]

{ #category : #markups }
MicroDownParser >> codeblockMarkup [ 
	^ self class codeblockMarkup 
]

{ #category : #markups }
MicroDownParser >> commentedLineMarkup [

	^ self class commentedLineMarkup
]

{ #category : #accessing }
MicroDownParser >> current [ 

	^ current
]

{ #category : #markups }
MicroDownParser >> environmentClosingBlockMarkup [
	^ self class environmentClosingBlockMarkup
]

{ #category : #markups }
MicroDownParser >> environmentOpeningBlockMarkup [
	^ self class environmentOpeningBlockMarkup
]

{ #category : #parsing }
MicroDownParser >> handleErrorInLine: line [
		
	^ MicParsingError new 
		line: line;
		signal
	
]

{ #category : #parsing }
MicroDownParser >> handleLine: line [
	"The logic is the following: 
		-first line is cleaned to avoid noise,
		-then if the current block can consume the line, it manages it and this potentially creates a new block that becomes the current one.
		When the line is not consumed, the current block is closed and its parent becomes the current one and the process is called back to treat the line."
		
	| normalized |
	normalized := line copyReplaceAll: String tab with: String space.
	[ 
	(current canConsumeLine: normalized)
		ifTrue: [ ^ current := current addLineAndReturnNextNode: normalized ]
		ifFalse: [ current closeMe ].
	current := current parent.
	self handleLine: normalized ] on: Error do: [ self handleErrorInLine: line ]
]

{ #category : #markups }
MicroDownParser >> headerMarkup [ 
	^ self class headerMarkup 
]

{ #category : #markups }
MicroDownParser >> horizontalLineMarkup [
	^ self class horizontalLineMarkup
]

{ #category : #initialization }
MicroDownParser >> initialize [

	super initialize.
	dispatchTable := Dictionary new.
	"should use a registration with classes, but later"
	dispatchTable at: self anchorMarkup put: MicAnchorBlock.
	dispatchTable at: self headerMarkup put: MicHeaderBlock.
	dispatchTable at: self codeblockMarkup put: MicCodeBlock.
	dispatchTable at: self annotatedParagraphMarkup put: MicAnnotatedBlock.
	dispatchTable at: self commentedLineMarkup put: MicCommentBlock.
	dispatchTable at: self horizontalLineMarkup put: MicHorizontalLineBlock.
	dispatchTable at: self mathOpeningBlockMarkup put: MicMathBlock.
	dispatchTable at: self environmentOpeningBlockMarkup put: MicEnvironmentBlock.
	dispatchTable at: self metaDataOpeningBlockMarkup put: MicMetaDataBlock.
	dispatchTable at: self unorderedListPlusMarkup put: MicUnorderedListBlock.
	dispatchTable at: self unorderedListMarkup put: MicUnorderedListBlock.
	dispatchTable at: self unorderedListStarMarkup put: MicUnorderedListBlock.
	dispatchTable at: self preformattedMarkup put: MicQuoteBlock.
	dispatchTable at: self tableCellMarkup put: MicTableBlock.
]

{ #category : #testing }
MicroDownParser >> isAList: normalized [
	
	^ (self matchUnordered: normalized) or: [ self matchOrdered: normalized ]
]

{ #category : #markups }
MicroDownParser >> italicMarkup [

	^ self class italicMarkup
]

{ #category : #'node creation' }
MicroDownParser >> matchOrdered: line [
	^ line prefixMatchesRegex: '\d+(\.|\))'
]

{ #category : #'node creation' }
MicroDownParser >> matchUnordered: line [
	^ ( line beginsWith: self unorderedListPlusMarkup ) 
		| ( line beginsWith: self unorderedListStarMarkup ) 
		| ( line beginsWith: self unorderedListMarkup )
]

{ #category : #markups }
MicroDownParser >> mathClosingBlockMarkup [
	^ self class mathClosingBlockMarkup
]

{ #category : #markups }
MicroDownParser >> mathOpeningBlockMarkup [
	^ self class mathOpeningBlockMarkup
]

{ #category : #markups }
MicroDownParser >> metaDataClosingBlockMarkup [
	^ self class metaDataClosingBlockMarkup
]

{ #category : #markups }
MicroDownParser >> metaDataOpeningBlockMarkup [
	^ self class metaDataOpeningBlockMarkup
]

{ #category : #parsing }
MicroDownParser >> newRootBlock [
	^ MicRootBlock new
]

{ #category : #markups }
MicroDownParser >> orderedListSemiMarkup [
	^ self class orderedListSemiMarkup
]

{ #category : #parsing }
MicroDownParser >> parse: aStreamOrString [
	"returns the root node of aStreamOrText"

	| inStream line |
	current := root := self newRootBlock
		setParser: self;
		yourself.
	inStream := aStreamOrString isStream 
		ifTrue:  [ aStreamOrString readStream ]
		ifFalse: [  aStreamOrString asString readStream ].
	[ line := inStream nextLine.
	line isNil ] whileFalse: [ self handleLine: line ].
	[ current = root ]
		whileFalse: [ current closeMe.
			current := current parent ].
	^ root
]

{ #category : #markups }
MicroDownParser >> preformattedMarkup [ 
	^ self class preformattedMarkup 
]

{ #category : #accessing }
MicroDownParser >> resolver [
	^ MicPathResolver new
]

{ #category : #accessing }
MicroDownParser >> setCurrent: aBlock [
	"aBlock parent = current 
		ifFalse: [ self error: 'When changing current, the current block should be the parent of the new one' ].
	note for me: we could just inforce this. aBlock parent: current.
	No this is not correct since 
	the parent of an item list is the list and this is the list that is parent of environment."	
	current := aBlock
]

{ #category : #markups }
MicroDownParser >> tableCellMarkup [

	^ self class tableCellMarkup
]

{ #category : #markups }
MicroDownParser >> unorderedListMarkup [
	^ self class unorderedListMarkup
]

{ #category : #markups }
MicroDownParser >> unorderedListPlusMarkup [

	^ self class unorderedListPlusMarkup
]

{ #category : #markups }
MicroDownParser >> unorderedListStarMarkup [

	^ self class unorderedListStarMarkup
]
