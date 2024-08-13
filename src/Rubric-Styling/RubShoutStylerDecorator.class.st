"
I am the shout styler decorator for a text area, I highlight the text 
"
Class {
	#name : 'RubShoutStylerDecorator',
	#superclass : 'RubParagraphDecorator',
	#instVars : [
		'styler',
		'text',
		'replaceStart',
		'replaceStop'
	],
	#category : 'Rubric-Styling',
	#package : 'Rubric-Styling'
}

{ #category : 'querying' }
RubShoutStylerDecorator class >> key [
	^ #shoutStyler
]

{ #category : 'private' }
RubShoutStylerDecorator >> aboutToBeUnplugged [

	self paragraph ifNotNil: [ :paragraph |
		paragraph textArea announcer unsubscribe: self
	]
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> beForSmalltalkScripting [
	self styler workspace: self model
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"

	self styler classOrMetaClass: aBehavior
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> defaultStyler [
	^SHRBTextStyler new view: self; yourself
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> next: aDecorator [
	super next: aDecorator.
	aDecorator ifNotNil: [
		self paragraph textArea announcer
			when: RubTextAcceptRequest
			send: #whenTextAcceptRequest:
			to: self
	]
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> okToStyle [
	^ self shoutEnabled and: [ self shoutAboutToStyle: self ]
]

{ #category : 'event handling' }
RubShoutStylerDecorator >> refreshStyling [
	"Shout for the whole text"	

	(next isNotNil and: [self okToStyle])
		ifTrue: [ self text: self text ]
]

{ #category : 'editing' }
RubShoutStylerDecorator >> replaceFrom: start to: stop with: aText [

	self okToStyle ifFalse: [ ^ next replaceFrom: start to: stop with: aText ].

	self paragraph disableDrawingWhile: [
		aText addAttribute: self paragraph defaultFontChange.
		text ifNil: [ text := self text ].
		text replaceFrom: start to: stop with: aText.
		replaceStart := start.
		replaceStop := stop.
		self style: text.
		next text: text.

		self paragraph ifNotNil: [ :paragraph |
			paragraph recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop - start + 1).
			paragraph textArea paragraphWasComposedFrom: start to: start + aText size - 1.
			paragraph textArea paragraphReplacedTextFrom: start to: stop with: aText ] ]
]

{ #category : 'shout' }
RubShoutStylerDecorator >> shoutAboutToStyle: myself [
	^ self paragraph textArea shoutAboutToStyle: myself
]

{ #category : 'private' }
RubShoutStylerDecorator >> shoutEnabled [
	^ SHPreferences enabled
]

{ #category : 'editing' }
RubShoutStylerDecorator >> style: aText [
	"we can do background styling with (self styler styleInBackgroundProcess: aText), but it is not safe"

	"on error just do nothing. Raising an error during styling may lead to an inconsistent
	state for the RubParagraph and its composer"

	[ ^ self styler style: aText ]
		on: Error
		do: [ :ex |
			ex traceCr.
			^ aText ]
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> styler [
	"Answer a <SHRBStyler>, responsible for highlighting text in the receiver. 
	Handle the case where `self model selectedClassOrMetaClass` is a RGBehavior (for example browsing unloaded code from Calypso)"

	^ styler ifNil: [
		  | selectedClassOrMeta |
		  styler := (self model isNotNil and: [ (selectedClassOrMeta := self model selectedClassOrMetaClass) isNotNil ])
			            ifTrue: [
				            self model selectedClassOrMetaClass syntaxHighlighter
					            view: self;
					            yourself ]
			            ifFalse: [ self defaultStyler ] ]
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> styler: aTextStyler [

	styler := aTextStyler.
	styler view: self
]

{ #category : 'editing' }
RubShoutStylerDecorator >> stylerStyled: styledCopyOfText [
	next ifNil: [ ^ self ].
	(text isNotNil and:[text string = styledCopyOfText string])
		ifFalse: [ ^ self ].
	text runs: styledCopyOfText runs.
	"must take care to recompose. If not, the textArea
	markIndex and pointIndex may be inconsistent"
	self paragraph
		recomposeFrom: replaceStart
		to: replaceStart + text size - 1
		delta: text size - (replaceStop - replaceStart + 1).

	"The selection and then the cursor bounds has to be updated
	to take possible text attribute changes into account"

	self updateTextArea
]

{ #category : 'editing' }
RubShoutStylerDecorator >> stylerStyledInBackground: styledCopyOfText [
	text ifNil: [ ^self ].
	self stylerStyled: styledCopyOfText
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> text [
	^ next text
]

{ #category : 'accessing' }
RubShoutStylerDecorator >> text: aText [

	self okToStyle ifFalse: [ ^ next text: aText ].
	self disableDrawingWhile: [
		aText addAttribute: self defaultFontChange.
		next text: (text := aText).
		replaceStart := 1.
		replaceStop := text size.
		self style: text ]
]

{ #category : 'private' }
RubShoutStylerDecorator >> updateTextArea [

	| textArea |

	textArea := self paragraph textArea.

	textArea recomputeSelection.
	textArea cursor updateBounds.

	(textArea scrollPane ifNil: [ textArea ]) changed
]

{ #category : 'event handling' }
RubShoutStylerDecorator >> whenTextAcceptRequest: anAnnouncement [

	self refreshStyling
]
