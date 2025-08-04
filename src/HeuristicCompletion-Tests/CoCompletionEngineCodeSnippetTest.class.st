Class {
	#name : 'CoCompletionEngineCodeSnippetTest',
	#superclass : 'OCCodeSnippetTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
CoCompletionEngineCodeSnippetTest >> testCompletion [
	"Smoke test for completion. Try to get completion entries on each position for each code snippet.
	This single test is a little slow (~15s) so it is in its own subclass."

	| editor controller completions |
	editor := RubSmalltalkEditor forTextArea: RubEditingArea new beForSmalltalkCode.
	controller := CoCompletionEngine new.
	controller setEditor: editor.
	editor completionEngine: controller.
	editor textArea model: CoCompletionEngineTest new.

	editor
		selectAll;
		addString: snippet source.
	self assert: editor string equals: snippet source.

	completions := (2 to: snippet source size + 1) collect: [ :i |
		editor selectFrom: i to: i - 1. "out carret"
		editor completionEngine createContext entries
			ifEmpty: [ nil ]
			ifNotEmpty: [ :e | e first contents ] ].
	self flag: 'do something with completion?'
]
