"
A ReturnValueDebugAction forces a return of a given value to the previous context

"
Class {
	#name : #ReturnValueDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
ReturnValueDebugAction class >> actionType [
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
ReturnValueDebugAction >> defaultLabel [

	^ 'Return entered value'
]

{ #category : #accessing }
ReturnValueDebugAction >> defaultOrder [

	^ 50
]

{ #category : #actions }
ReturnValueDebugAction >> executeAction [
	|expression value|
	
	expression := UIManager default request: 'Enter expression for return value:'.
	value := self class compiler
			source: expression;
			context: self currentContext;
			receiver: self currentContext receiver;
			evaluate.

	self session returnValue: value from: self currentContext
]

{ #category : #accessing }
ReturnValueDebugAction >> help [
	^ 'Return of a given value to the previous context.'
]

{ #category : #accessing }
ReturnValueDebugAction >> id [

	^ #returnValue
]
