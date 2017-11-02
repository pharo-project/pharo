"
A PreDebugDoesNotUnderstandAction adds the DoesNotUnderstandDebugAction debugging actions to the pre debug window. When executed, it will further close the pre debug window and open the full debugger. 

Instance Variables
	preDebugWindow:		<Object>

preDebugWindow
	- xxxxx

"
Class {
	#name : #PreDebugDoesNotUnderstandAction,
	#superclass : #DoesNotUnderstandDebugAction,
	#instVars : [
		'preDebugWindow'
	],
	#category : 'DebuggerActions'
}

{ #category : #registration }
PreDebugDoesNotUnderstandAction class >> actionType [
	<preDebuggingAction>
]

{ #category : #actions }
PreDebugDoesNotUnderstandAction >> executeAction [

	"The create method from the session will trigger #contextChanged, which will
	make the debgger update the stack. Sof it safe to just open the debugger."
	super executeAction.
	self preDebugWindow openFullDebugger
]

{ #category : #accessing }
PreDebugDoesNotUnderstandAction >> id [

	^ #preDebugDoesNotUnderstand
]

{ #category : #initialization }
PreDebugDoesNotUnderstandAction >> initialize [
	
	super initialize.
	self needsUpdate: false.
	self needsValidation: false
]

{ #category : #accessing }
PreDebugDoesNotUnderstandAction >> preDebugWindow [
	
	^ preDebugWindow
]

{ #category : #accessing }
PreDebugDoesNotUnderstandAction >> preDebugWindow: anObject [
	
	preDebugWindow := anObject
]
