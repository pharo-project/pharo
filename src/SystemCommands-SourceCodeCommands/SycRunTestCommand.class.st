Class {
	#name : 'SycRunTestCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'converting' }
SycRunTestCommand >> asRefactorings [

	^ {  }
]

{ #category : 'testing' }
SycRunTestCommand >> canBeExecuted [

	^ self context isMethodSelected not and: [ self context isTestMethodSelected ]
]

{ #category : 'accessing' }
SycRunTestCommand >> description [

	^ 'Runs test.'
]

{ #category : 'execution' }
SycRunTestCommand >> executeRefactoring [

	| selector testClasses |
	selector := self method selector.
	testClasses := self method methodClass isAbstract
					   ifTrue: [ self method methodClass allSubclasses ]
					   ifFalse: [ { self method methodClass } ].

	testClasses do: [ :concreteClass |
		| testResult |
		testResult := concreteClass debug: selector.
		testResult updateResultsInHistory.
		GrowlMorph showTestResult: testResult with: selector ]
]

{ #category : 'accessing' }
SycRunTestCommand >> name [
	^ 'Run test'
]

{ #category : 'accessing' }
SycRunTestCommand >> order [

	^ 1.4
]
