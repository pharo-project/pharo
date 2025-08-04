"
Tests for HowToMakeYourOwnTutorial
"
Class {
	#name : 'HowToMakeYourOwnTutorialTest',
	#superclass : 'TutorialTest',
	#category : 'ProfStef-Tests-Tests',
	#package : 'ProfStef-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
HowToMakeYourOwnTutorialTest >> testedTutorial [
	"Returns an instance of an AbstractTutorial subclass"
	^ HowToMakeYourOwnTutorial new
]
