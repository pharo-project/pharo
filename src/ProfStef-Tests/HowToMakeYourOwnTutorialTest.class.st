"
Tests for HowToMakeYourOwnTutorial
"
Class {
	#name : #HowToMakeYourOwnTutorialTest,
	#superclass : #TestCase,
	#traits : 'TTutorialTest',
	#classTraits : 'TTutorialTest classTrait',
	#category : 'ProfStef-Tests-Tests'
}

{ #category : #tests }
HowToMakeYourOwnTutorialTest >> testEachSelectorExists [
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector) 
	]
]

{ #category : #tests }
HowToMakeYourOwnTutorialTest >> testedTutorial [
	"Returns an instance of an AbstractTutorial subclass"
	^ HowToMakeYourOwnTutorial new
]
