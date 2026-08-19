Class {
	#name : 'OverCommand',
	#superclass : 'StepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
OverCommand >> initialize [

	super initialize.
	self commandName: #( 'over' 'o' ).
	self commandHelp: 'execute next stepOver'.
	self arguments: ''
]

{ #category : 'printing' }
OverCommand >> performStepOn: aDebugger [
    aDebugger session stepOver
]

{ #category : 'printing' }
OverCommand >> printResultFor: aDebugger [
    ^ 'Stepped OVER to: ' , aDebugger context printString , ' (PC: ' , aDebugger context pc asString , ')'
]
