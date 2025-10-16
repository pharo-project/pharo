Class {
	#name : 'FastStepThroughTest',
	#superclass : 'StepThroughTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'utilities' }
FastStepThroughTest >> settingUpSessionAndProcessAndContextForBlock: block [

	super settingUpSessionAndProcessAndContextForBlock: block.
	currentFastStepThrough := DebugSession fastStepThroughMode.
	DebugSession fastStepThroughMode: true.
	session := DebugSession named: 'test session' on: process startedAt: context
]
