"
```
ClySyntaxHelpMorph new openInWorld 
```
"
Class {
	#name : #ClySyntaxHelpMorph,
	#superclass : #ClyStatusBarItemMorph,
	#instVars : [
		'button'
	],
	#category : #'Microdown-Calypso-Morph'
}

{ #category : #building }
ClySyntaxHelpMorph >> action [

	| window text | 
	window := SystemWindow labelled: 'Microdown Syntax Summary'.
	window extent: 800@600.
	text := self buildText.
	text setText: self syntax.
	window addMorph: text frame: (0 @ 0 corner: 1 @ 1).
	window openInWorld
]

{ #category : #building }
ClySyntaxHelpMorph >> build [
	
	button := PluggableButtonMorph
		on: self 
		getState: nil
		action:  #action 
		label: #label 
		menu: nil. 	
	self addMorph: button
]

{ #category : #building }
ClySyntaxHelpMorph >> buildText [ 
 
	| textModel scrolledText |
	textModel := RubScrolledTextModel new interactionModel: self.
	scrolledText := textModel newScrolledText
		beReadOnly;
		beWrapped;
		yourself.
	scrolledText textArea font: StandardFonts codeFont.
	^ scrolledText
]

{ #category : #accessing }
ClySyntaxHelpMorph >> label [
	^ 'Syntax Help'
]

{ #category : #building }
ClySyntaxHelpMorph >> rawMicrodownSyntax [
	"should use the textual builder!!!"
	^ '

*** 
Horizontal 
*** 

#Header


```language=Pharo&caption=Beautiful&label=Fig1
   1000 factorial / 999 factorial
```

![Pharologo](https://files.pharo.org/media/logo/logo.png|size=80&caption=Our great figure.)

A link: [http://pharo.org](http://pharo.org)

- item 1
  1. sub item 1 
  3. sub item 2
- item 2


**Bold**, _italic_, `monospace`

In Pharo hyperlinks to: 
- class `Point`, 
- method `Point class`, `Point>>#setX:setY:`, and 
- package `#’Microdown-Tests’ (for packages).

You can edit this file clicking on `ClySyntaxHelpMorph>>#rawMicrodownSyntax`.
'
]

{ #category : #building }
ClySyntaxHelpMorph >> syntax [ 

	"^ 'The Pharo in image version offers: header, paragraph, codeblock, lists, hyperlinks and many format (bold, monospace, italic, links, .... A larger version is available for books.', String cr, 'The following is rendered below.' asText, String cr, String cr, self syntaxRaw, String cr, ([MicroDownParser asText: self syntaxRaw ] on: MicParsingError, MessageNotUnderstood  do: [ '' ])"
	
	"^ 'The Pharo in image version offers: header, paragraph, codeblock, lists, hyperlinks and many format (bold, monospace, italic, links, .... A larger version is available for books.', String cr, 'The following is rendered below.' asText, String cr asText, String cr asText, self rawMicrodownSyntax, String cr asText, (MicroDownParser asText: self rawMicrodownSyntax )"
	
	^ ('The Pharo in image version offers: header, paragraph, codeblock, lists, hyperlinks and many format (bold, monospace, italic, links, .... A larger version is available for books.', String cr, 'The following is rendered below.', String cr, String cr, self rawMicrodownSyntax, String cr) asText, ([MicroDownParser asText: self rawMicrodownSyntax ] on: MicParsingError, MessageNotUnderstood  do: [ '' ])
]
