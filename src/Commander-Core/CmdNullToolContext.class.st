"
I allow to use CmdCommandActivator in cases when command is created manually without any tool.
In that case activation has no context and command is prepared by user manually.
"
Class {
	#name : #CmdNullToolContext,
	#superclass : #CmdToolContext,
	#category : #'Commander-Core'
}

{ #category : #testing }
CmdNullToolContext >> allowsExecutionOf: aCommandClass [

	^true
]

{ #category : #'command execution' }
CmdNullToolContext >> applyResultOf: aCommand [

]

{ #category : #'command execution' }
CmdNullToolContext >> prepareFullExecutionOf: aCommand [

]

{ #category : #'command execution' }
CmdNullToolContext >> prepareNewCommand: aCommand [

]
