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

{ #category : 'helper' }
EnhancedStepThroughTest >> stepA1 [
	self evalBlockThenReturnOne: [ self stepA2 ]
]

{ #category : 'helper' }
EnhancedStepThroughTest >> stepA2 [
	^ 2+2
]

{ #category : 'helper' }
EnhancedStepThroughTest >> stepB1 [
	self stepB2.
	self stepB3
]

{ #category : 'helper' }
EnhancedStepThroughTest >> stepB2 [
	^ 42
]

{ #category : 'helper' }
EnhancedStepThroughTest >> stepB3 [
	^ 43
]
