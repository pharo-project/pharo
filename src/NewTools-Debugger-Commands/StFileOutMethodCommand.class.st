"
Requests the debugger to file out the method of the selected context in the stack
"
Class {
	#name : #StFileOutMethodCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StFileOutMethodCommand class >> defaultDescription [
	^ 'Write the source code of the selected method on a file.'
]

{ #category : #initialization }
StFileOutMethodCommand class >> defaultIconName [
	^ #smallExport
]

{ #category : #accessing }
StFileOutMethodCommand class >> defaultName [	
	^ 'File out method'
]

{ #category : #initialization }
StFileOutMethodCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StFileOutMethodCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #actions }
StFileOutMethodCommand >> execute [
	self debugger fileOutSelectedContext
]
