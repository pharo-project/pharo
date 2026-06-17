Class {
	#name : 'StepCommand',
	#superclass : 'StepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
StepCommand >> initialize [

	super initialize.
	self commandName: #( 'step' 's' ).
	self commandHelp: 'execute next stepInto'.
	self arguments: ''
]

{ #category : 'printing' }
StepCommand >> performStepOn: aDebugger [
    aDebugger session stepInto
]

{ #category : 'printing' }
StepCommand >> printResultFor: aDebugger [

	^ 'Stepped to: ' , aDebugger context printString , '(PC: ' , aDebugger context pc printString , ')'
]
