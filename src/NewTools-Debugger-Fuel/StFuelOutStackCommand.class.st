"
Serializes a portion of the current stack trace using fuel.
"
Class {
	#name : #StFuelOutStackCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Fuel'
}

{ #category : #accessing }
StFuelOutStackCommand class >> defaultDescription [
	^ 'Serialize a portion of the current stack trace using fuel.'
]

{ #category : #initialization }
StFuelOutStackCommand class >> defaultIconName [
	^ #smallExport
]

{ #category : #accessing }
StFuelOutStackCommand class >> defaultName [	
	^ 'Fuel out stack'
]

{ #category : #initialization }
StFuelOutStackCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StFuelOutStackCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #actions }
StFuelOutStackCommand >> execute [

	FLDebuggerStackSerializer new serializeStackFromContext:
		self context interruptedContext copyStack
]
