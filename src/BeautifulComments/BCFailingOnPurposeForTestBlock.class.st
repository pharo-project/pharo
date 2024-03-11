"
I am a comment which cannot be rendered correctly - I use a dedicated error to fail. 
Toggle Edit/View comment to see how this is done.
{!failingOnPurpose!}
"
Class {
	#name : 'BCFailingOnPurposeForTestBlock',
	#superclass : 'MicAnnotationBlock',
	#classVars : [
		'FailOnPurpose'
	],
	#category : 'BeautifulComments-Tests',
	#package : 'BeautifulComments',
	#tag : 'Tests'
}

{ #category : 'accessing' }
BCFailingOnPurposeForTestBlock class >> failOnPurpose [
	FailOnPurpose ifNil: [ FailOnPurpose := false ].
	^ FailOnPurpose 
]

{ #category : 'accessing' }
BCFailingOnPurposeForTestBlock class >> failOnPurpose: aBoolean [
	FailOnPurpose := aBoolean
]

{ #category : 'accessing' }
BCFailingOnPurposeForTestBlock class >> tag [
	^ #failingOnPurpose
]

{ #category : 'visiting' }
BCFailingOnPurposeForTestBlock >> accept: aVisitor [
	self class failOnPurpose 
		ifTrue: [ BCFailingOnPurposeForTestError signal ]
]
