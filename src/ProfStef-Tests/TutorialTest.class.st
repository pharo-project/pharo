Class {
	#name : 'TutorialTest',
	#superclass : 'TestCase',
	#category : 'ProfStef-Tests-Tests',
	#package : 'ProfStef-Tests',
	#tag : 'Tests'
}

{ #category : 'testing' }
TutorialTest class >> isAbstract [

	^self = TutorialTest
]

{ #category : 'tests' }
TutorialTest >> testEachSelectorExists [
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector)
	]
]

{ #category : 'tests' }
TutorialTest >> testEachSelectorReturnsALesson [
	| answer |
	self testedTutorial tutorial do: [:aSelector|
		answer := (self testedTutorial perform: aSelector).
		self assert: (answer isKindOf: Lesson).
	]
]

{ #category : 'tests' }
TutorialTest >> testLessonAtReturnsCorrespondingLesson [
	| answer |
	1 to: (self testedTutorial tutorial size) do: [:index|
		answer := self testedTutorial lessonAt: index.
		self assert: (answer isKindOf: Lesson)
	]
]

{ #category : 'tests' }
TutorialTest >> testNotEmpty [
	self assert: self testedTutorial tutorial notEmpty
]

{ #category : 'tests' }
TutorialTest >> testSizeReturnsNumberOfSelectors [
	self assert: (self testedTutorial tutorial size) equals: self testedTutorial size
]
