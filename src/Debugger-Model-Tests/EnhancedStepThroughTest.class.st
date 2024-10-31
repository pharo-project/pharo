Class {
	#name : 'EnhancedStepThroughTest',
	#superclass : 'StepThroughTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'utilities' }
EnhancedStepThroughTest >> settingUpSessionAndProcessAndContextForBlock: aBlock [
	super settingUpSessionAndProcessAndContextForBlock: aBlock.
	session := process newEnhancedDebugSessionNamed: 'test session' startedAt: context
]
