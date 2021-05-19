"
A FuelOutStackDebugAction is	a debugging action that serialize a portion of the current stack trace using fuel.

"
Class {
	#name : #FuelOutStackDebugAction,
	#superclass : #DebugAction,
	#category : #'NewTools-Debugger-Fuel'
}

{ #category : #registration }
FuelOutStackDebugAction class >> actionType [
	<contextMenuDebuggingAction>
]

{ #category : #'gt-extensions' }
FuelOutStackDebugAction class >> gtActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			order: 30
]

{ #category : #serialization }
FuelOutStackDebugAction class >> serializeStackFromContext: aContext [
	FLDebuggerStackSerializer serializeStackFromContext: aContext
]

{ #category : #accessing }
FuelOutStackDebugAction >> defaultLabel [

	^ 'Fuel out Stack'
]

{ #category : #accessing }
FuelOutStackDebugAction >> defaultOrder [

	^ 125
]

{ #category : #actions }
FuelOutStackDebugAction >> executeAction [
	self class serializeStackFromContext: self interruptedContext
]

{ #category : #accessing }
FuelOutStackDebugAction >> help [
	^ 'Serialize a portion of the current stack trace using fuel.'
]

{ #category : #accessing }
FuelOutStackDebugAction >> id [

	^ #fuelOutStack
]
