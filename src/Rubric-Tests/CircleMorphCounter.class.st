Class {
	#name : 'CircleMorphCounter',
	#superclass : 'CircleMorph',
	#instVars : [
		'counter'
	],
	#category : 'Rubric-Tests-Base',
	#package : 'Rubric-Tests',
	#tag : 'Base'
}

{ #category : 'accessing' }
CircleMorphCounter >> counter [
	counter ifNil: [ counter := 0 ].
	^ counter
]

{ #category : 'accessing' }
CircleMorphCounter >> counter: anObject [

	counter := anObject
]

{ #category : 'drawing' }
CircleMorphCounter >> fullDrawOn: aCanvas [
	super fullDrawOn: aCanvas.
	counter := self counter + 1
]
