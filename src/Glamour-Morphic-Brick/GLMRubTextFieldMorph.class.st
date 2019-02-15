"
A GLMRubTextFieldMorph is xxxxxxxxx.

Instance Variables
	customHeight:		<Object>

customHeight
	- xxxxx

Raw functions to center textarea:

	""for 16: 20,29 - 4""
	""newTop := ((self height - self textArea height) / 2.0).
	newTop := newTop truncated abs.""
	
	""for 15: 20,28 - 3""
	""newTop := ((self height - self textArea height) / 2.0) + 1.
	newTop := newTop truncated abs.""
	
	""for 14: 20,26 - 2""
	""newTop := ((self height - self textArea height) / 2.0) + 1.
	newTop := newTop truncated abs.""
	
	""for 13: 20,24 - 1""
	""newTop := ((self height - self textArea height) / 2.0) + 1.
	newTop := newTop truncated abs.""
	
	""for 12: 20,23 - 1""
	""newTop := ((self height - self textArea height) / 2.0).
	newTop := newTop truncated abs.""
	
	""for 11:  20,21 - 0""
	""newTop := ((self height - self textArea height) / 2.0).
	newTop := newTop truncated abs.""
	
	""for 10:  20,19 - 0""
	""newTop := ((self height - self textArea height) / 2.0).
	newTop := newTop truncated.""
	
	""for 9: 20, 18 - 0""
	""newTop := ((self height - self textArea height) / 2.0) - 1.
	newTop := newTop truncated.""
	
	""for 8: 20, 16 - 1""
	""newTop := ((self height - self textArea height) / 2.0) - 1.
	newTop := newTop truncated.""
	
	""for 7: 20, 14 - 2""
	""newTop := ((self height - self textArea height) / 2.0) - 1.
	newTop := newTop truncated abs.""
"
Class {
	#name : #GLMRubTextFieldMorph,
	#superclass : #RubTextFieldMorph,
	#instVars : [
		'customHeight'
	],
	#category : #'Glamour-Morphic-Brick-Morphic'
}

{ #category : #geometry }
GLMRubTextFieldMorph >> extent: aPoint [
	|newTop patch offset|
	super extent: aPoint.
	
	offset := (self height - self textArea height) / 2.0.
	patch := offset fractionPart rounded - offset sign.
	newTop := (offset + patch) truncated abs.
	
	self textArea top: newTop.
]

{ #category : #geometry }
GLMRubTextFieldMorph >> forceHeight: anObject [

	customHeight := anObject.
]

{ #category : #initialization }
GLMRubTextFieldMorph >> newScrollPane [
	"using hacked ScrollPane to remove focus border"
	| sp |
	sp := GLMRubTextScrollPane new.
	sp scrollTarget: self defaultScrollTarget.
	^ sp
]

{ #category : #geometry }
GLMRubTextFieldMorph >> textFieldHeight [

	^ customHeight
		ifNil: [ self textArea height ]
		ifNotNil: [ customHeight brickValue: self ]
]
