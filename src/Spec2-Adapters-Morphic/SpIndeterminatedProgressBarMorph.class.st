"
Description
--------------------

I am a Progress bar with an indeterminated advance. I'll just show a bar going back and forth.

Examples
--------------------

IndeterminatedProgressBarMorph new
	extent: 200@6;
	openInWorld.	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	oddPassage:		<aBoolean>		For design purpose, one passage of the loading is faster that the other. This boolean is used to know if the current passage should be fast or slow.

"
Class {
	#name : #SpIndeterminatedProgressBarMorph,
	#superclass : #ProgressBarMorph,
	#instVars : [
		'oddPassage'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #example }
SpIndeterminatedProgressBarMorph class >> example [
	<script>
	self new
		extent: 200 @ 6;
		openInWorld
]

{ #category : #drawing }
SpIndeterminatedProgressBarMorph >> drawOn: aCanvas [
	"Draw the receiver with the fill style for the bar, clipping to the inner bounds."

	| area |
	area := self innerBounds.
	aCanvas fillRectangle: area fillStyle: self fillStyle.
	value > 0
		ifTrue: [ cachedWidth := self completedWidth.
			"Compared to my superclass, we do not always start at the origin of the progress bar since it's not a determinated progress"
			area := (area left + (cachedWidth - (area width / 2.5))) @ area top corner: area origin + (cachedWidth @ area height).
			aCanvas fillRectangle: area fillStyle: self barFillStyle ].
	self borderStyle frameRectangle: self bounds on: aCanvas
]

{ #category : #'stepping and presenter' }
SpIndeterminatedProgressBarMorph >> indeterminateStepIncrease [
	"To get a feeling of recent design we use a progression with two timing and we switch the speed at each passage."

	^ oddPassage ifTrue: [ 40 ] ifFalse: [ 15 ]
]

{ #category : #initialization }
SpIndeterminatedProgressBarMorph >> initialize [
	self flag: #pharoTodo. "I think this class should be merged in ProgressBarMorph in Pharo 8."
	super initialize.
	oddPassage := true
]

{ #category : #'stepping and presenter' }
SpIndeterminatedProgressBarMorph >> step [
	"At each step we are increasing the value of the progress bar. If we get at the end, we juts begin once again."

	| range |
	super step.

	range := end - start.

	self value: self value + (range / self indeterminateStepIncrease).

	self value > range ifFalse: [ ^ self ].

	self value: start.
	oddPassage := oddPassage not
]

{ #category : #'stepping and presenter' }
SpIndeterminatedProgressBarMorph >> stepTime [
	^ 20
]
