Class {
	#name : 'EnhancedStepThroughWithHaltingBlockTest',
	#superclass : 'StepThroughTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'utilities' }
EnhancedStepThroughWithHaltingBlockTest >> settingUpSessionAndProcessAndContextForBlock: aBlock [

	super settingUpSessionAndProcessAndContextForBlock: aBlock.
	session :=  EnhancedDebugSessionWithHaltingBlock named: 'test session' on: process startedAt: context
	

]
