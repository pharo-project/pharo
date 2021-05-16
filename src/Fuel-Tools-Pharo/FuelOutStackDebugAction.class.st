"
A FuelOutStackDebugAction is	a debugging action that serialize a portion of the current stack trace using fuel.

"
Class {
	#name : #FuelOutStackDebugAction,
	#superclass : #DebugAction,
	#category : #'Fuel-Tools-Pharo-Debugger'
}

{ #category : #registration }
FuelOutStackDebugAction class >> actionType [
	<contextMenuDebuggingAction>
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
