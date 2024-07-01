"
I implement the `Syntax help` button in the lower right of this pane: 

![image](https://user-images.githubusercontent.com/17723745/138031552-9f8c5653-1b9c-4553-8aa4-0fe0ff75faf8.png).


"
Class {
	#name : 'ClySyntaxHelpMorph',
	#superclass : 'ClyStatusBarItemMorph',
	#instVars : [
		'button'
	],
	#category : 'BeautifulComments-Core',
	#package : 'BeautifulComments',
	#tag : 'Core'
}

{ #category : 'building' }
ClySyntaxHelpMorph >> action [

	| window text | 
	window := SystemWindow labelled: 'Microdown Syntax Summary'.
	window extent: 400@300.
	text := self buildText.
	text setText: self syntax.
	window addMorph: text frame: (0 @ 0 corner: 1 @ 1).
	window openInWorld
]

{ #category : 'building' }
ClySyntaxHelpMorph >> build [
	
	button := PluggableButtonMorph
		on: self 
		getState: nil
		action:  #action 
		label: #label 
		menu: nil. 	
	self addMorph: button
]

{ #category : 'building' }
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

{ #category : 'accessing' }
ClySyntaxHelpMorph >> label [
	^ 'Syntax Help'
]

{ #category : 'building' }
ClySyntaxHelpMorph >> rawMicrodownSyntax [

	^ '
#Microdown is quite cool

Here is some code 

```language=Pharo&caption=Beautiful&anchor=Fig1
   1000 factorial / 999 factorial
```

Here is a figure and a link: [http://pharo.org](http://pharo.org).

![Pharologo](https://files.pharo.org/media/logo/logo.png size=80&anchor=figLogo.)



Here is a list:
- item 1
  1. sub item 1 
  3. sub item 2
- item 2


**Bold**, _italic_, `monospace`

In Pharo, Microdown supports hyperlinks to: 
- classes e.g., `Point`, 
- methodes e.g., `Point class`, `Point>>#setX:setY:`, and 
- packages e.g., `#''Microdown-Tests''` (for packages).

You can edit this file clicking on `ClySyntaxHelpMorph>>#rawMicrodownSyntax`.
'
]

{ #category : 'building' }
ClySyntaxHelpMorph >> syntax [ 
	"Return a text to be rendered by a morph, and produced by the MicRichTextComposer"
	
	^ (MicRichTextComposer asRichText: self rawMicrodownSyntax , String cr, String cr, 'Microdown''s in image version offers: header, paragraph, codeblock, lists, hyperlinks and many format (bold, monospace, italic, links, .... A larger version is available for books.', String cr) , ('Here is the corresponding text in Microdown.', String cr, String cr, self rawMicrodownSyntax, String cr) asText
]
