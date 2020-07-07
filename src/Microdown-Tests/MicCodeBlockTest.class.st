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
	#category : #'Microdown-Tests'
}

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

{ #category : #tests }
MicCodeBlockTest >> testContents [
	| source root textBody argument |
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := parser parse: source.
	self assert: root children first contents equals: '[ my fancy code
is really cool ]'
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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
	self assert: (code arguments at:#caption) equals: 'Getting a morph for an instance of ==Color=='.
	self assert: (code arguments at:#label) equals: 'scr:asMorphInColor'.
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
