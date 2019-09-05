"
SUnit tests for tutorial player
"
Class {
	#name : #TutorialPlayerTutorialAccessorTest,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #tests }
TutorialPlayerTutorialAccessorTest >> testDefaultsToSmalltalkSyntaxTutorial [
	| player |
	player := TutorialPlayer new.
	self assert: (player tutorial isKindOf: PharoSyntaxTutorial).
]

{ #category : #tests }
TutorialPlayerTutorialAccessorTest >> testWithMockTutorial [
	| mockTutorial player |
	mockTutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: mockTutorial; yourself.
	self assert: player tutorial equals: mockTutorial.
]
