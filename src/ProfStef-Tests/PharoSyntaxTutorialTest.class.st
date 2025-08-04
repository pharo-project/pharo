"
SUnit tests for PharoSyntaxTutorial
"
Class {
	#name : 'PharoSyntaxTutorialTest',
	#superclass : 'TutorialTest',
	#category : 'ProfStef-Tests-Tests',
	#package : 'ProfStef-Tests',
	#tag : 'Tests'
}

{ #category : 'requirements' }
PharoSyntaxTutorialTest >> testedTutorial [
	"Returns an instance of an AbstractTutorial subclass"
	^ PharoSyntaxTutorial new
]
