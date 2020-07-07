"
I can keep track of the count of nested structures so each structure as its own number. For exemple, 2 sections in Chapter 1 would have a counter of #(1 1) and #(1 2). The first number indicates the top-most structure (here a chapter), while the following numbers represent the nested structures. I can count up to a max level.
"
Class {
	#name : #PRCascadingCounter,
	#superclass : #Object,
	#instVars : [
		'counters'
	],
	#category : #'Pillar-Core-Counters'
}

{ #category : #'instance creation' }
PRCascadingCounter class >> maxLevel: anInteger [
	^ self new 
		setMaxLevel: anInteger;
		yourself
]

{ #category : #accessing }
PRCascadingCounter >> current [
	^ self deepCopy
]

{ #category : #action }
PRCascadingCounter >> dontIncrementAt: level [ 
	counters at: level put: -1
]

{ #category : #accessing }
PRCascadingCounter >> elements [
	^ counters first: self indexOfLastNonZero
]

{ #category : #action }
PRCascadingCounter >> incrementAt: level [ 
	counters at: level put: (counters at: level) + 1.
	level + 1 to: counters size
		do: [ :i | counters at: i put: 0 ]
]

{ #category : #accessing }
PRCascadingCounter >> indexOfLastNonZero [
	counters size to: 1 by: -1 do: [ :index | 
		(counters at: index) strictlyPositive
			ifTrue: [ ^ index ] ].
	^ 1
]

{ #category : #testing }
PRCascadingCounter >> isValidCounter [
	^ true
]

{ #category : #printing }
PRCascadingCounter >> printOn: aStream [
	self elements do: [ :each | aStream nextPutAll: each printString ] separatedBy: [ aStream nextPut: $. ]
]

{ #category : #initialization }
PRCascadingCounter >> setMaxLevel: anInteger [
	counters := Array new: anInteger withAll: 0.
]
