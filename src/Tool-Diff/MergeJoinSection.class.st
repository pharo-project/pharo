"
A join section usable for merging
"
Class {
	#name : #MergeJoinSection,
	#superclass : #JoinSection,
	#instVars : [
		'selectedBorderColor',
		'selected',
		'selectionState',
		'stateIcons',
		'allowClick'
	],
	#category : #'Tool-Diff-Joins'
}

{ #category : #accessing }
MergeJoinSection >> allowClick [
	"Answer the value of allowClick"

	^ allowClick
]

{ #category : #accessing }
MergeJoinSection >> allowClick: anObject [
	"Set the value of allowClick"

	allowClick := anObject
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> appendToCompositeText: aText [
	"If appropriate append the relevant src or dst text to the given text."
	
	self selectionState == #src ifTrue: [^aText append: self src text].
	self selectionState == #dst ifTrue: [^aText append: self dst text].
	self selectionState == #both ifTrue: [
		^aText
			append: self src text;
			append: self dst text]
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> borderColorToUse [
	"Answer the border color to use based on the selection state."
	
	^self selected
		ifTrue: [self selectedBorderColor]
		ifFalse: [super borderColorToUse]
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> clicked [
	"The receiver or a highlight was clicked."

	self wantsClick ifFalse: [^false].
	self selectNextState.
	^true
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> containsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	^(super containsPoint: aPoint) or: [
		self stateIcon notNil and: [self stateIconBounds containsPoint: aPoint]]
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> defaultStateIcons [
	"Answer the default state icons."

	^ {(self iconNamed: #smallBackIcon) . (self iconNamed: #smallForwardIcon) .
	(self iconNamed: #smallOkIcon) . (self iconNamed: #smallCancelIcon)}
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> drawOn: aCanvas [
	"Draw the join on the given canvas."
	
	super drawOn: aCanvas.
	self stateIcon ifNotNil: [:i |
		aCanvas
			translucentImage: i
			at: self stateIconBounds topLeft]
]

{ #category : #initialization }
MergeJoinSection >> initialize [
	"Initialize the receiver."

	self
		allowClick: true;
		selected: false;
		selectionState: #dst;
		stateIcons: self defaultStateIcons;
		selectedBorderColor: Color black.
	super initialize
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> selectNextState [
	"Set the selection state to the next one with wraparound."

	self selectionState: (
		self selectionStates at: (
			(self selectionStates indexOf: self selectionState) \\
				self selectionStates size + 1))
]

{ #category : #accessing }
MergeJoinSection >> selected [
	"Answer the value of selected"

	^ selected
]

{ #category : #accessing }
MergeJoinSection >> selected: aBoolean [
	"Set the value of selected"

	selected := aBoolean.
	self
		updateHighlights;
		changed: #selected
]

{ #category : #accessing }
MergeJoinSection >> selectedBorderColor [
	"Answer the value of selectedBorderColor"

	^ selectedBorderColor
]

{ #category : #accessing }
MergeJoinSection >> selectedBorderColor: anObject [
	"Set the value of selectedBorderColor"

	selectedBorderColor := anObject
]

{ #category : #accessing }
MergeJoinSection >> selectionState [
	"Answer the value of selectionState"

	^ selectionState
]

{ #category : #accessing }
MergeJoinSection >> selectionState: anObject [
	"Set the value of selectionState"

	selectionState := anObject
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> selectionStates [
	"Answer the valid selection states in order."

	^#(src dst both neither)
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> stateIcon [
	"Answer the state icon to use."

	self wantsClick ifFalse: [^nil].
	^self stateIcons at: (self selectionStates indexOf: self selectionState)
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> stateIconBounds [
	"Answer the bounds of the state icon."
	
	|i|
	i := self stateIcon ifNil: [^nil].
	^self shape bounds center - (i extent // 2)
		extent: i extent
]

{ #category : #accessing }
MergeJoinSection >> stateIcons [
	"Answer the value of stateIcons"

	^ stateIcons
]

{ #category : #accessing }
MergeJoinSection >> stateIcons: anObject [
	"Set the value of stateIcons"

	stateIcons := anObject
]

{ #category : #'as yet unclassified' }
MergeJoinSection >> wantsClick [
	"Allow if explictly enabled and super."

	^self allowClick and: [super wantsClick]
]
