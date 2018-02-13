"
The implementation who asumes that my model it's a nautilus object.
"
Class {
	#name : #SugsNautilusContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Nautilus'
}

{ #category : #refactoring }
SugsNautilusContext >> browsedEnvironment [
	^ model browsedEnvironment
]

{ #category : #refactoring }
SugsNautilusContext >> code [

	^ self sourceTextArea getString
]

{ #category : #refactoring }
SugsNautilusContext >> contentSelection [
	^ model contentSelection
]

{ #category : #refactoring }
SugsNautilusContext >> flashSourceCodeArea [
	^ model flashSourceCodeArea
]

{ #category : #refactoring }
SugsNautilusContext >> formatSourceCode [
	| formatted |
	formatted := selectedNode formattedCode.
	formatted = self code
		ifTrue: [ ^ self ].
	self sourceTextArea
		formatSourceCodeInView;
		hasUnacceptedEdits: true
]

{ #category : #refactoring }
SugsNautilusContext >> selectTheTextForTheNode [
	selectionPreviousHighligth := self selectedInterval.
	selectedNode ifNotNil: [ self contentSelection ]
]

{ #category : #accessing }
SugsNautilusContext >> selectedClass [
	^ model selectedClass
]

{ #category : #accessing }
SugsNautilusContext >> selectedInterval [

	^ model selectionInterval
]

{ #category : #refactoring }
SugsNautilusContext >> selectedMethod [
	^ model selectedMethod
]

{ #category : #refactoring }
SugsNautilusContext >> selectionInterval [
	^ model selectionInterval
]

{ #category : #refactoring }
SugsNautilusContext >> sourceTextArea [
	^ model sourceTextModel
]

{ #category : #refactoring }
SugsNautilusContext >> window [
	^ model window 
]
