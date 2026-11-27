"
I check how Rubric's Smalltalk editor takes its completion engine, with the IDE's completion engines.
"
Class {
	#name : 'RubSmalltalkEditorCompletionTest',
	#superclass : 'TestCase',
	#instVars : [
		'currentCompletion'
	],
	#category : 'HeuristicCompletion-Tests-Rubric',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Rubric'
}

{ #category : 'running' }
RubSmalltalkEditorCompletionTest >> setUp [

	super setUp.
	currentCompletion := RubSmalltalkEditor completionEngineClass
]

{ #category : 'running' }
RubSmalltalkEditorCompletionTest >> tearDown [

	RubSmalltalkEditor completionEngineClass: currentCompletion.
	super tearDown
]

{ #category : 'tests' }
RubSmalltalkEditorCompletionTest >> testCompletionEngineInstanceIsNotUsedIfNil [

	| current |
	[
		current := RubSmalltalkEditor completionEngineInstance.
		RubSmalltalkEditor completionEngineInstance: nil.
		RubSmalltalkEditor completionEngineClass: CoStatisticsCompletionEngine.
		self assert: RubSmalltalkEditor new completionEngine class equals: CoStatisticsCompletionEngine ] ensure: [
		RubSmalltalkEditor completionEngineInstance: current ]
]

{ #category : 'tests' }
RubSmalltalkEditorCompletionTest >> testCompletionEngineInstanceIsUsedIfSpecified [

	| current |
	[
		current := RubSmalltalkEditor completionEngineInstance.
		RubSmalltalkEditor completionEngineInstance: CoStatisticsCompletionEngine new.
		self assert: RubSmalltalkEditor new completionEngine class equals: CoStatisticsCompletionEngine ] ensure: [
		RubSmalltalkEditor completionEngineInstance: current ]
]

{ #category : 'tests - completion' }
RubSmalltalkEditorCompletionTest >> testSettingCompletionFromEditor [

	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	self assert: RubSmalltalkEditor completionEngineClass equals: CompletionEngine
]

{ #category : 'tests - completion' }
RubSmalltalkEditorCompletionTest >> testSettingCompletionFromEditorToTextArea [

	| textEditor |
	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	textEditor := RubSmalltalkEditor new.
	self assert: textEditor completionEngine class equals: CompletionEngine.
	textEditor textArea: RubEditingArea new.
	self assert: textEditor textArea completionEngine class equals: CompletionEngine
]
