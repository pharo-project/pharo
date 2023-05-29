"
I am an Abstract class.
Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them.
They may perform their styling asynchronously, in a background process which I create and manage.

My public interface is...

	view: aViewOrMorph - set the view that will receive notifications when styling has completed.
	
	format: aText - modifies aText's string

	style: aText - modifies the TextAttributes of aText, but does not change the string, then sends #stylerStyled: to the view.

	styledTextFor: aText - answers a formatted and styled copy of aText

	unstyledTextFrom: aText - answers a copy of aText with all TextAttributes removed

Subclasses of me should re-implement...

	privateFormat: aText - answer a formatted version of aText; the String may be changed
	privateStyle: aText - modify the TextAttributes of aText; but do not change the String
	

	
	

"
Class {
	#name : #SHTextStyler,
	#superclass : #Object,
	#instVars : [
		#text,
		#view,
		#stylingEnabled
	],
	#category : #'Shout-Styling'
}

{ #category : #formatting }
SHTextStyler >> format: aText [

	^ aText
]

{ #category : #initialization }
SHTextStyler >> initialize [

	super initialize.
	stylingEnabled := true
]

{ #category : #private }
SHTextStyler >> privateStyle: aText [

	self shouldBeImplemented
]

{ #category : #styling }
SHTextStyler >> style: aText [

	aText ifEmpty: [ ^ self ].
	stylingEnabled ifFalse: [ ^ self ].
	text := aText copy.
	self privateStyle: text.
	view ifNotNil: [ :view2 | view2 stylerStyled: text ]
]

{ #category : #styling }
SHTextStyler >> styledTextFor: aText [

	^ self privateStyle: aText
]

{ #category : #private }
SHTextStyler >> stylingEnabled [

	^ stylingEnabled
]

{ #category : #private }
SHTextStyler >> stylingEnabled: aBoolean [

	stylingEnabled := aBoolean
]

{ #category : #styling }
SHTextStyler >> unstyledTextFrom: aText [

	^ Text fromString: aText string
]

{ #category : #accessing }
SHTextStyler >> view: aViewOrMorph [

	view := aViewOrMorph
]
