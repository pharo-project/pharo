Class {
	#name : #ClassUsedInInstructionStreamTest,
	#superclass : #SuperClassUsedInInstructionStreamTest,
	#instVars : [
		'expectedValue'
	],
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #example }
ClassUsedInInstructionStreamTest >> aSuperMethod: anInteger with: anotherInteger [

	^ self error
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> expectedValue [

	^ expectedValue
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> methodWithASuperBlock [

	^ [ super aSuperMethod: 1 with: 2 ]
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> methodWithASuperBlockWithoutArguments [

	^ [ super yourself ]
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> valueOfBlockWithSupersend [

	expectedValue := self methodWithASuperBlock value
]
