"
A RubEditingAreaTest is a test class for testing the behavior of RubEditingArea
"
Class {
	#name : #RubEditingAreaTest,
	#superclass : #RubAbstractTextAreaTest,
	#instVars : [
		'area',
		'position',
		'event'
	],
	#category : #'Rubric-Tests'
}

{ #category : #'instance creation' }
RubEditingAreaTest >> newCharacterBlockIndex: anInteger text: aText [
	| line |
	line := (RubTextLine start: 1 stop: 33 internalSpaces: 0 paddingWidth: 0) rectangle: (0 @ 0 corner: 0 @ 0).
	^ (CharacterBlock new stringIndex: anInteger text: aText topLeft: 0@0 extent: 0 @ 0)
			textLine: line;
			yourself
]

{ #category : #convenience }
RubEditingAreaTest >> sendDoubleClickAt: index withShift: aBoolean [
	| buttons |
	position := (area paragraph characterBlockForIndex: index) surface leftCenter.
	buttons := (aBoolean ifTrue: [8] ifFalse: [0]).
	buttons := buttons bitOr: MouseButtonEvent redButton.
	event := MouseButtonEvent basicNew 
					setType: #mouseDown
					position: position
					which: MouseButtonEvent redButton
					buttons: buttons
					hand: (HandMorph new mouseFocus: area; yourself)
					stamp: nil.
	^ area doubleClick: event
]

{ #category : #convenience }
RubEditingAreaTest >> sendMouseClickAt: index withShift: aBoolean [
	| buttons |
	position := (area paragraph characterBlockForIndex: index) surface leftCenter.
	buttons := (aBoolean ifTrue: [8] ifFalse: [0]).
	buttons := buttons bitOr: MouseButtonEvent redButton.
	area simulateClickWith: buttons position: position.
	
]

{ #category : #convenience }
RubEditingAreaTest >> setSelectionFrom: startIndex to: stopIndex text: aText [
	area markIndex: stopIndex pointIndex: startIndex
]

{ #category : #running }
RubEditingAreaTest >> setUp [
	super setUp.
	area := RubEditingArea new.
	area setTextWith: 'one two three four'.
]

{ #category : #'tests-accessing selection' }
RubEditingAreaTest >> testMarkBlockPointBlock [
	self setSelectionFrom: 3 to: 6 text: area text.
	self assert: area pointBlock stringIndex equals: 3.
	self assert: area markBlock stringIndex equals: 6
]

{ #category : #'tests-accessing selection' }
RubEditingAreaTest >> testModifySelection [
	self testMarkBlockPointBlock.
	area modifySelection: (self newCharacterBlockIndex: 8 text: area text).
	self assert: area markBlock stringIndex equals: 3.
	self assert: area pointBlock stringIndex equals: 8.
]

{ #category : #'tests-accessing selection' }
RubEditingAreaTest >> testMouseClickShiftClick [
	self sendMouseClickAt: 3 withShift: false.
	self sendMouseClickAt: 6 withShift: true.
	self assert: area markBlock stringIndex equals: 3.
	self assert: area pointBlock stringIndex equals: 6
]

{ #category : #'tests-accessing selection' }
RubEditingAreaTest >> testMouseMoveAfterDoubleClick [
	self sendMouseClickAt: 11 withShift: false.
	self sendDoubleClickAt: 11 withShift: false.
	self sendMouseClickAt: 17 withShift: true.
	self assert: area markBlock stringIndex equals: 9.
	self assert: area pointBlock stringIndex equals: 17.
]
