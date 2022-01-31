"
I'm is a test class for testing the behavior of MicCodeBlock


For now we do not support the use of multiple backticks. 

If you want to embed codeblock inside codeblock you must use an extra space. 

```
 ```
 A nested codeblock
 ```
```
"
Class {
	#name : #MicCodeBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #'tests - arguments' }
MicCodeBlockTest >> testAssociations [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source code associations |
	source := parser codeblockMarkup, 'language=pharo&caption=`Color` is a class.&label=scr:asMorphInColor', String cr, 'Color >> asMorph
    ^ Morph new color: self', String cr, parser codeblockMarkup. 

	code := (self parser parse: source) children first.
	associations :=  { ('language' -> 'pharo') . ('caption' -> { MicMonospaceFormatBlock new substring: 'Color' . MicTextBlock new substring: ' is a class.' }) . ('label' -> 'scr:asMorphInColor') }.
	
	self 
		assert: code argumentAssociations first 
		equals: associations first.
	self 
		assert: code argumentAssociations second value first value substring
		equals: associations second value first substring.
	self 
		assert: code argumentAssociations second value first value kind
		equals: associations second value first kind.	
		
	self 
		assert: code argumentAssociations third 
		equals: associations third
]

{ #category : #tests }
MicCodeBlockTest >> testBody [
	| source root textBody |
	textBody := 'my fancy code
is really cool'.
	source := parser codeblockMarkup, String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	self assert: root children first body equals: textBody.
	
]

{ #category : #tests }
MicCodeBlockTest >> testCanConsumeLine [ 
	"This test verfies that when a code block is closed it does not consume anymore."
	"It also show that consume is not just blindly do a line starts with the markup 
	since often the first line is consumed by the parent of the block."
	
	| source root textBody argument |
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	
	"An already created blockNode cannot consume a new code block since no nesting is allowed."
	self deny: (root children first canConsumeLine: '```language=zork').
	
	"An already closed blockNode cannot consume even plain text"
	self deny: (root children first canConsumeLine: 'plain text')
]

{ #category : #tests }
MicCodeBlockTest >> testCodeBlockWithNestedCodeBlock [
	"This test shows that we can write a code block inside a code block but that the later is not interpreter."
	
	| source root code textBody |
	textBody := ' ``` 
 my fancy code
 is really cool
 ```'.
	source := parser codeblockMarkup,  String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	
	self 
		assert: root children size 
		equals: 1.
	code := root children first.
	self assert: code children isEmpty.
	self
		assert: code code
		equals: textBody.
	
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testFirstTagNoValue [

	"
	```language&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code firstTag equals: 'language'.
	self assert: (code arguments at:#language) isNil
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testFirstTagNoValueButContainSpace [
	"this is not really good but right now we have it like that."
	"
	```lang uage
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'lang uage'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code firstTag equals: 'lang uage'.
	self assert: (code arguments at:#'lang uage') isNil
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testFirstTagValue [

	| source codeBlock |
	source := parser codeblockMarkup, 'language=12', String cr, 'Color >> asMorph
    ^ Morph new color: self' , String cr, parser codeblockMarkup. 
   codeBlock :=(self parser parse: source) children first.
	self assert:  codeBlock firstTagValue equals: '12'.
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testFirstTagWithValue [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language=PharoMethod'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code firstTag equals: #language.
	self assert: (code arguments at:#language) equals: 'PharoMethod'
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testFourBackQuotes [

	| codeBlock |
	codeBlock := parser parse: '```` 
something in the way
````'.

	self assert: codeBlock children first body equals: 'something in the way'
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testFourBackQuotesClosedByThreeQuotes [

	| codeBlock |
	codeBlock := parser parse: '```` 
something in the way
```'.

	self assert: codeBlock children first body equals: 'something in the way'
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testFourBackQuotesOnMultipleLines [

	| codeBlock |
	codeBlock := parser parse: '````pharo
something in the way
pif
glop or pas glop
````'.

	self assert: codeBlock children first body equals: 'something in the way
pif
glop or pas glop'
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testFourBackQuotesOnMultipleLinesWithText [

	| codeBlock |
	codeBlock := parser parse: '
Before	
	
	
````pharo
something in the way
pif
glop or pas glop
````
After
'.

	self assert: codeBlock children second body equals: 'something in the way
pif
glop or pas glop'
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testFourBackQuotesWithTextAround [

	| codeBlock |
	codeBlock := parser parse: '
Start text here
	
```` 
something in the way
````

Ending text'.

	self assert: codeBlock children second body equals: 'something in the way'
]

{ #category : #tests }
MicCodeBlockTest >> testHasCaption [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	
	code := root children first.
	self assert: code hasCaption
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testHasFirstTag [

	| source codeBlock |
	source := parser codeblockMarkup, 'language=PharoMethod', String cr, 'Color >> asMorph
    ^ Morph new color: self' , String cr, parser codeblockMarkup. 
   codeBlock :=(self parser parse: source) children first.
	self assert:  codeBlock hasFirstTag.
	self assert:  codeBlock firstTag equals: 'language'.
]

{ #category : #'tests - first tag' }
MicCodeBlockTest >> testHasFirstTagWithNoFirstTag [

	| source codeBlock |
	source := parser codeblockMarkup, String cr,  'my fancy code
is really cool', String cr, parser codeblockMarkup. 
	codeBlock := (parser parse: source) children first.
	self assert: codeBlock firstTagValue equals: ''
]

{ #category : #tests }
MicCodeBlockTest >> testHasNOCaption [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language=pharo&label=scr:asMorphInColor'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	
	code := root children first.
	self deny: code hasCaption
]

{ #category : #tests }
MicCodeBlockTest >> testNoFirstTag [

	"
	```
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := ''.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code firstTag equals: ''.
	self assert: code arguments isEmpty
]

{ #category : #tests }
MicCodeBlockTest >> testOpenCanConsumeLine [
	| source root textBody argument line code |
	
	"ready for code factory"
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := (parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup) readStream. 
	
	line := source nextLine.
	
	"parsing an empty line make sure that we get the root block node"
	root := parser parse: ''.
	self assert: parser current equals: root.
	
	"now the root block can consume ```xxxx"
	self assert: (root canConsumeLine: line).
	"now we get back an open code block"
	code := parser handleLine: line.
	self assert: code parent equals: root.
	self deny: code isClosed. 
	
	"now new line"
	line := source nextLine.
	self assert: (code canConsumeLine: line). 
	self assert: (code canConsumeLine: parser codeblockMarkup).
	"now a code block is closed."
	self deny: (code canConsumeLine: line). 
	self assert: code isClosed
	
	
	
]

{ #category : #tests }
MicCodeBlockTest >> testSingleBlockWithArguments [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code code equals: textBody.
	self assert: (code arguments at:#language) equals: 'pharo'.
	self assert: code captionElements first substring equals: 'Getting a morph for an instance of ==Color=='.
	self assert: (code arguments at:#label) equals: 'scr:asMorphInColor'.
]

{ #category : #tests }
MicCodeBlockTest >> testSingleBlockWithArgumentsWithFormat [

	"
	```language=pharo&caption=Getting a morph for an instance of ==Color==&label=scr:asMorphInColor
Color >> asMorph
    ^ Morph new color: self
	```
	"
	| source root code  textBody argument |
	textBody := 'Color >> asMorph
    ^ Morph new color: self'.
	argument := 'language=pharo&caption=`Color` is a class.&label=scr:asMorphInColor'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.

	
	code := root children first.
	self assert: code code equals: textBody.
	self assert: code firstTag equals: #language.
	self assert: (code arguments at: #language) equals: 'pharo'.
	
	self assert: code caption equals: 'Color is a class.'.
	self assert: code captionElements first kind equals: 'monospace'.
	self assert: code captionElements first substring equals: 'Color'.
	
	self assert: code captionElements second kind equals: 'basic'.
	self assert: code captionElements second substring equals: ' is a class.'.
	
	self assert: code label equals: 'scr:asMorphInColor'.
]

{ #category : #tests }
MicCodeBlockTest >> testSingleCodeBlock [
	| source root code  textBody argument |
	textBody := 'my fancy code'.
	argument := 'Argument'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	self assert: root children size equals: 1.
	code := root children first.
	self assert: code code equals: textBody.
	self assert: (code arguments at: #Argument) isNil.
	self assert: code firstTag equals: argument
]

{ #category : #tests }
MicCodeBlockTest >> testSingleCodeBlockListInside [
	"This test shows that list inside a codeblock are not managed as list but as plain text."
	
	| source root code textBody argument |
	textBody := '* my fancy code
* is really cool'.
	argument := 'ArgumentGoesHere'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	
	self 
		assert: root children size 
		equals: 1.
	code := root children first.
	self assert: code children isEmpty.
	self
		assert: code code
		equals: textBody.
	self 
		assert: (code arguments at: #ArgumentGoesHere)
		equals: nil
]

{ #category : #tests }
MicCodeBlockTest >> testSingleCodeBlockTwoLines [
	| source root code textBody argument |
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	self 
		assert: root children size 
		equals: 1.
	code := root children first.
	self
		assert: code code
		equals: textBody.
	self 
		assert: (code arguments at: #ArgumentGoesHere) isNil.
	self assert: code firstTag equals: argument
]

{ #category : #'tests - deviation' }
MicCodeBlockTest >> testThreeBackQuotesClosedByFourQuotes [

	| codeBlock |
	codeBlock := parser parse: '``` 
something in the way
````'.

	self assert: codeBlock children first body equals: 'something in the way'
]

{ #category : #tests }
MicCodeBlockTest >> testedClass [ 
	^ MicCodeBlock 
]
