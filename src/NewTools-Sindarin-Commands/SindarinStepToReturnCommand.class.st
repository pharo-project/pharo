Class {
	#name : #SindarinStepToReturnCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinStepToReturnCommand class >> defaultDescription [
	^ 'Step to just before the current context returns, then returns debugger control.'
]

{ #category : #initialization }
SindarinStepToReturnCommand class >> defaultIconName [
	^ #glamorousRightSide
]

{ #category : #default }
SindarinStepToReturnCommand class >> defaultName [
	<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>
	^ 'Step to return'
]

{ #category : #executing }
SindarinStepToReturnCommand >> execute [ 	
	self flag: 'Not nice to always force update of debugger -> use observer?'.
	self context sindarinDebugger stepToReturn.
	self context forceSessionUpdate
]
