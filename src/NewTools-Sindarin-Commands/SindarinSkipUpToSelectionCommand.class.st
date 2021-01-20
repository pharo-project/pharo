Class {
	#name : #SindarinSkipUpToSelectionCommand,
	#superclass : #SindarinSkipAllToSelectionCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #'as yet unclassified' }
SindarinSkipUpToSelectionCommand class >> defaultDescription [
	^ 'Skips execution and stops before the selected instruction (or the instruction preceding the cursor), then returns debugger control.'
]

{ #category : #'as yet unclassified' }
SindarinSkipUpToSelectionCommand class >> defaultName [
	"<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>"
	^ '[Exp.] Skip up to (stop before)'
]

{ #category : #hooks }
SindarinSkipUpToSelectionCommand >> execute [
	| targetNode |
	
	self flag: 'Context should actually be a debugger or a sindarin debugger'.
	self flag: 'Suspicious call to internal debugger UI state'.
	targetNode := self context sindarinDebugger
		bestNodeFor: self context code nonEmptySelectionInterval.
	self context sindarinDebugger skipUpToNode: targetNode.
	self context forceSessionUpdate
]
