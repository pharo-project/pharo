"
See EditingState comments.
SmalltalkEditingState is made to manage data for Smalltalk code editing sessions (such as a selector chooser).

Instance Variables:
   selectorChooser <SelectorChooserMorph>

- selectorChooser
The basic selector chooser which is popup to fetch available selectors or class names begining with a prefix. The prefix is token from the current caret location (see SelectorChooserMorph comments).

"
Class {
	#name : #SmalltalkEditingState,
	#superclass : #EditingState,
	#instVars : [
		'selectorChooser'
	],
	#category : #'Text-Edition'
}

{ #category : #'selector-chooser' }
SmalltalkEditingState >> chosenSelector: aSelector for: aTextMorph [
	| newStart replacement  caret prevSelection |
	self closeSelectorChooser.
	aSelector ifNil: [^ self].
	aTextMorph editor selectPrecedingIdentifier.
	prevSelection := aTextMorph selection.
	newStart := aTextMorph editor startIndex.
	caret := aTextMorph editor stopIndex.
	replacement := String streamContents: [:strm | | kwds |
		1 to: (kwds := aSelector keywords) size 
		do: [:i | strm nextPutAll: (kwds at: i).
			i = 1 ifTrue: [caret := newStart + strm contents size + 1].
			strm nextPutAll: (i < kwds size ifTrue: ['  '] ifFalse: [' '])]].
	prevSelection asString = replacement trimBoth 
		ifTrue: [aTextMorph flash]
		ifFalse: [aTextMorph replaceSelectionWith: replacement]. 
	aTextMorph selectFrom: caret to: caret - 1.
	(aTextMorph respondsTo: #editView) ifTrue: [aTextMorph editView selectionInterval: aTextMorph editor selectionInterval].

]

{ #category : #'selector-chooser' }
SmalltalkEditingState >> closeSelectorChooser [
	selectorChooser 
		ifNotNil: [ [selectorChooser delete] ensure: [selectorChooser := nil] ]
]

{ #category : #events }
SmalltalkEditingState >> focused: aBoolean from: aTextMorph [
	super focused: aBoolean from: aTextMorph.
	selectorChooser ifNotNil: [selectorChooser comeToFront]
]

{ #category : #events }
SmalltalkEditingState >> handlesKeyboard: evt from: aTextMorph [
	^ (super handlesKeyboard: evt from: aTextMorph)
		or: [selectorChooser notNil and: [selectorChooser handlesKeyboard: evt]]
]

{ #category : #events }
SmalltalkEditingState >> keystroke: aKeyboardEvent from: aTextMorph [
	| asciiValue |
	selectorChooser 
		ifNotNil: [(selectorChooser handlesKeyStroke: aKeyboardEvent) 
			ifTrue: [selectorChooser keyStroke: aKeyboardEvent.
				^ true]].
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue
		ifTrue: [self closeSelectorChooser.
			^ true].
	aKeyboardEvent controlKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true].
	"aKeyboardEvent commandKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true]."
	self updateSelectorChooserFrom: aTextMorph.
	^ false
]

{ #category : #events }
SmalltalkEditingState >> mouseDown: anEvent from: aTextMorph [
	super mouseDown: anEvent from: aTextMorph.
	self closeSelectorChooser
]

{ #category : #'selector-chooser' }
SmalltalkEditingState >> querySymbolFrom: aTextMorph [
	| pref |

	aTextMorph editor hasSelection
		ifFalse: [aTextMorph editor selectPrecedingIdentifier].
	pref := String streamContents: [:strm |
		aTextMorph selection asString trimBoth do: [:c | c isSeparator ifFalse: [strm nextPut: c]]].
	selectorChooser := SelectorChooserMorph popUpPrefix: pref from: aTextMorph.

]

{ #category : #'selector-chooser' }
SmalltalkEditingState >> selectorChooser [
	^ selectorChooser
]

{ #category : #'selector-chooser' }
SmalltalkEditingState >> smartQuerySymbolFrom: aTextMorph [
	| sel pref caret nextPref |
	sel := aTextMorph editor selectionInterval.
	aTextMorph editor selectPrecedingIdentifier.
	pref := aTextMorph selection asString.
	selectorChooser := SelectorChooserMorph popUpPrefix: pref from: aTextMorph.
	nextPref := selectorChooser 
		ifNotNil: [ selectorChooser nextPrefixWith: pref ].
	nextPref 
		ifNil: [aTextMorph editor selectInterval: sel]
		ifNotNil: [	selectorChooser prefix: nextPref.
			aTextMorph editor selectPrecedingIdentifier.
			aTextMorph editor replaceSelectionWith: nextPref.
			caret := aTextMorph editor selectionInterval first + nextPref size.
			aTextMorph editor selectInterval: (caret to: caret - 1)]
]

{ #category : #'selector-chooser' }
SmalltalkEditingState >> updateSelectorChooserFrom: aTextMorph [
	selectorChooser ifNil: [^ self].
	self closeSelectorChooser.
	self querySymbolFrom: aTextMorph
]
