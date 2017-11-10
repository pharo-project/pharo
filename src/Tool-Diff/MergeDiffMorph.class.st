Class {
	#name : #MergeDiffMorph,
	#superclass : #DiffMorph,
	#instVars : [
		'allowJoinClicks'
	],
	#category : #Tool-Diff
}

{ #category : #accessing }
MergeDiffMorph >> allowJoinClicks [
	"Answer the value of allowJoinClicks"

	^ allowJoinClicks
]

{ #category : #accessing }
MergeDiffMorph >> allowJoinClicks: anObject [
	"Set the value of allowJoinClicks"

	allowJoinClicks := anObject
]

{ #category : #actions }
MergeDiffMorph >> calculatedJoinMappings [
	"Specify click allowance for each section."
	
	^super calculatedJoinMappings do: [:j |
		j allowClick: self allowJoinClicks]
]

{ #category : #actions }
MergeDiffMorph >> compositeText [
	"Answer the composite text based on the selection state
	of the joins."

	^self joinMorph compositeText
	
]

{ #category : #obsolete }
MergeDiffMorph >> indicateDst [
	"Change the indicators of the joins to the dst side."

	self joinMappings do: [:section |
		section selectionState: #dst].
	self
		changed;
		changed: #selectedDifferences
]

{ #category : #obsolete }
MergeDiffMorph >> indicateSrc [
	"Change the indicators of the joins to the src side."

	self joinMappings do: [:section |
		section selectionState: #src].
	self
		changed;
		changed: #selectedDifferences
]

{ #category : #initialization }
MergeDiffMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		allowJoinClicks: true
]

{ #category : #actions }
MergeDiffMorph >> joinSectionClass [
	"Answer the class to use for a new join section."

	^MergeJoinSection
]

{ #category : #actions }
MergeDiffMorph >> newJoinMorph [
	"Answer a new join morph."

	^super newJoinMorph
		when: #joinClicked send: #update: to: self with: #joinClicked
]

{ #category : #actions }
MergeDiffMorph >> update: aspect [
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #joinClicked
		ifTrue: [self
				changed;
				changed: #selectedDifferences]
]
