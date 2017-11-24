"
Helper class for MorphTest
"
Class {
	#name : #TestInWorldMorph,
	#superclass : #Morph,
	#instVars : [
		'intoWorldCount',
		'outOfWorldCount'
	],
	#category : #'Morphic-Tests-Kernel'
}

{ #category : #initialization }
TestInWorldMorph >> initialize [
	super initialize.
	outOfWorldCount := intoWorldCount := 0.
]

{ #category : #accessing }
TestInWorldMorph >> intoWorld: aWorld [
	aWorld ifNil:[^self].
	super intoWorld: aWorld.
	intoWorldCount := intoWorldCount + 1.

]

{ #category : #accessing }
TestInWorldMorph >> intoWorldCount [
	^intoWorldCount
]

{ #category : #accessing }
TestInWorldMorph >> outOfWorld: aWorld [
	aWorld ifNil:[^self].
	super outOfWorld: aWorld.
	outOfWorldCount := outOfWorldCount + 1.

]

{ #category : #accessing }
TestInWorldMorph >> outOfWorldCount [
	^outOfWorldCount
]
