"
SUnit tests for tutorial player
"
Class {
	#name : #TutorialPlayerTestTutorialAccessor,
	#superclass : #TestCase,
	#category : 'ProfStef-Tests-Tests'
}

{ #category : #tests }
TutorialPlayerTestTutorialAccessor >> testDefaultsToSmalltalkSyntaxTutorial [
	| player |
	player := TutorialPlayer new.
	self assert: (player tutorial isKindOf: PharoSyntaxTutorial).
]

{ #category : #tests }
TutorialPlayerTestTutorialAccessor >> testWithMockTutorial [
	| mockTutorial player |
	mockTutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: mockTutorial; yourself.
	self assert: player tutorial equals: mockTutorial.
]
