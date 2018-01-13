"
SUnit tests for the Smart suggestions context
"
Class {
	#name : #SugsContextTest,
	#superclass : #TestCase,
	#category : #'SmartSuggestions-Tests-Tests'
}

{ #category : #'tests - valid context' }
SugsContextTest >> testDebuggerIsAValidContext [
	| debugger |
	
	debugger := Smalltalk tools debugger new.
	self assert: (SugsAbstractContext contextFor: debugger) class equals: SugsDebuggerContext .
]

{ #category : #'tests - valid context' }
SugsContextTest >> testTetModelIsAValidContext [
	| textModel |
	
	(textModel := TextPresenter new)
		beForCode.
	self assert: (SugsAbstractContext contextFor: textModel) class equals: SugsMessageBrowserContext.
]
