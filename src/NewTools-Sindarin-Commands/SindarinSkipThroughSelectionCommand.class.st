Class {
	#name : #SindarinSkipThroughSelectionCommand,
	#superclass : #SindarinSkipAllToSelectionCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #'as yet unclassified' }
SindarinSkipThroughSelectionCommand class >> defaultDescription [
	^ 'Skips execution and stops after the selected instruction (or the instruction preceding the cursor), then returns debugger control.'
]

{ #category : #'as yet unclassified' }
SindarinSkipThroughSelectionCommand class >> defaultName [
	"<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>"
	"^ 'Skip through instruction (will stop after)'"
	^ '[Exp.] Skip through (stop after)'
]

{ #category : #hooks }
SindarinSkipThroughSelectionCommand >> execute [
	| targetNode |
	self
		flag: 'Context should actually be a debugger or a sindarin debugger'.
	self flag: 'Suspicious call to internal debugger UI state'.
	targetNode := self context sindarinDebugger
		bestNodeFor: self context code nonEmptySelectionInterval.
	self context sindarinDebugger skipThroughNode: targetNode.
	self context forceSessionUpdate
]
