"
Trait for testing tutorials
"
Trait {
	#name : #TTutorialTest,
	#category : #'ProfStef-Tests-Traits'
}

{ #category : #tests }
TTutorialTest >> testEachSelectorExists [
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self tutorial respondsTo: aSelector) 
	]
]

{ #category : #tests }
TTutorialTest >> testEachSelectorReturnsALesson [
	| answer |
	self testedTutorial tutorial do: [:aSelector|
		answer := (self testedTutorial perform: aSelector).
		self assert: (answer isKindOf: Lesson).
	]
]

{ #category : #tests }
TTutorialTest >> testLessonAtReturnsCorrespondingLesson [
	| answer |
	1 to: (self testedTutorial tutorial size) do: [:index|
		answer := self testedTutorial lessonAt: index.
		self assert: (answer isKindOf: Lesson)
	]
	
]

{ #category : #tests }
TTutorialTest >> testNotEmpty [
	self assert: self testedTutorial tutorial notEmpty.
]

{ #category : #tests }
TTutorialTest >> testSizeReturnsNumberOfSelectors [
	self assert: (self testedTutorial tutorial size) equals: self testedTutorial size.
]

{ #category : #requirements }
TTutorialTest >> testedTutorial [
	"Returns an instance of an AbstractTutorial subclass"
	^ self explicitRequirement
]
