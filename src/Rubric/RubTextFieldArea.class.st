"
I am a TextArea that only has one line of text.
"
Class {
	#name : #RubTextFieldArea,
	#superclass : #RubAbstractTextArea,
	#category : #'Rubric-Editing-Core'
}

{ #category : #defaults }
RubTextFieldArea >> defaultMargins [

	^ Margin left: 2 right: 2 top: 0 bottom: 1
]

{ #category : #private }
RubTextFieldArea >> newParagraph [
	^ RubExtraSelectionDecorator next: super newParagraph

]

{ #category : #private }
RubTextFieldArea >> privateReplaceFrom: start to: stop with: aText [

	super privateReplaceFrom: start to: stop with: (self sanitize: aText)
]

{ #category : #private }
RubTextFieldArea >> sanitize: aText [
	| lineEndings |
	lineEndings := { Character cr. Character lf }.
	"Remove line endings that may have been introduced via copy-paste, but only if we have to because it will remove all text attributes"
	^ (aText includesAnyOf: lineEndings)
		ifTrue: [ aText copyWithoutAll: lineEndings ]
		ifFalse: [ aText ]
]

{ #category : #settings }
RubTextFieldArea >> walkAlongDisplayedLine [
	^ false
]
