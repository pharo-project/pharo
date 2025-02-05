"
SUnit tests for PharoSyntaxTutorial
"
Class {
	#name : 'PharoSyntaxTutorialTest',
	#superclass : 'TestCase',
	#traits : 'TTutorialTest',
	#classTraits : 'TTutorialTest classTrait',
	#category : 'ProfStef-Tests-Tests',
	#package : 'ProfStef-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
PharoSyntaxTutorialTest >> testEachSelectorExists [
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector)
	]
]

{ #category : 'requirements' }
PharoSyntaxTutorialTest >> testedTutorial [
	"Returns an instance of an AbstractTutorial subclass"
	^ PharoSyntaxTutorial new
]
