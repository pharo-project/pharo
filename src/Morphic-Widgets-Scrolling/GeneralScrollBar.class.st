"
Support for GeneralScrollPane.
"
Class {
	#name : #GeneralScrollBar,
	#superclass : #ScrollBar,
	#instVars : [
		'showState'
	],
	#category : #Morphic-Widgets-Scrolling
}

{ #category : #initialization }
GeneralScrollBar >> initialize [
	"Initialize the receiver."

	super initialize.
	self showWhenNeeded
]

{ #category : #'model access' }
GeneralScrollBar >> setValue: newValue [
	"Bypass screwed up scrollbar!"
	
	^self perform: #setValue: withArguments: {newValue} inSuperclass: Slider
]

{ #category : #accessing }
GeneralScrollBar >> showAlways [
	"Set the scrollbar to be always shown."

	self showState: #always
]

{ #category : #accessing }
GeneralScrollBar >> showNever [
	"Set the scrollbar to be never shown."

	self showState: #never
]

{ #category : #accessing }
GeneralScrollBar >> showState [

	^ showState
]

{ #category : #accessing }
GeneralScrollBar >> showState: anObject [

	showState := anObject
]

{ #category : #accessing }
GeneralScrollBar >> showWhenNeeded [
	"Set the scrollbar to be shown if needed."

	self showState: #whenNeeded
]

{ #category : #accessing }
GeneralScrollBar >> showsAlways [
	"Answer whether the scrollbar should always be shown."

	^self showState == #always
]

{ #category : #accessing }
GeneralScrollBar >> showsNever [
	"Answer whether the scrollbar should never be shown."

	^self showState == #never
]

{ #category : #accessing }
GeneralScrollBar >> showsWhenNeeded [
	"Answer whether the scrollbar should be shown if needed."

	^self showState == #whenNeeded
]
