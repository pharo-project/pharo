Class {
	#name : 'SycRunTestCommand',
	#superclass : 'SycTestCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'activation' }
SycRunTestCommand class >> methodEditorShorcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation
		  by: $e meta , $t meta
		  for: ClyTextEditorContext
]

{ #category : 'converting' }
SycRunTestCommand >> asRefactorings [

	^ {  }
]

{ #category : 'accessing' }
SycRunTestCommand >> defaultMenuItemName [
	^ 'Run test'
]

{ #category : 'accessing' }
SycRunTestCommand >> description [

	^ 'Runs test.'
]

{ #category : 'converting' }
SycRunTestCommand >> execute [

	| selector testClasses |
	selector := method selector.
	testClasses := method methodClass isAbstract
		               ifTrue: [ method methodClass allSubclasses ]
		               ifFalse: [ { method methodClass } ].

	testClasses do: [ :concreteClass |
		| testResult |
		testResult := concreteClass debug: selector.
		testResult updateResultsInHistory.
		GrowlMorph showTestResult: testResult with: selector ]
]

{ #category : 'testing' }
SycRunTestCommand >> isComplexRefactoring [

	^ false
]
