"
I am a root of application context hierarchy. I am bound to concrete tool (widget. element) of application.
My subclasses provide specific information about tool/application state.
They are used to declare command activation strategies. Every activation is created for concrete context class.

###TODO: 
Instances of my subclasses are used for command lookup. Each activation strategy checks that given context instance is activation of declared context class:
	aToolContext isActivationOf: toolContextClass 
By default I am activation of any of my superclasses. If command activation is defined for most base context class like me (CmdToolContext) then such command will be available for any kind of command tools: any shotcuts lookup, any menu, etc..
Subclasses can override this method to extend set of commands which should be available for them but which declared for other context classes.

I also responsible for command activation:
- allowsExecutionOf: aCommand
- prepareNewCommand: aCommand
- prepareFullExecutionOf: aCommand
- applyResultOf: aCommand

I delegate these messages to command with idea that my default implementation is kind of standard context. For example: 
	CmdToolContext>prepareFullExecutionOf: aCommand
		aCommand prepareFullExecutionInContext: self
My subclasses can override these method to ask commands for specific set of activation messages.

I provide comparison method #isSimilarTo: to compare two context instances. It can be usefull to detect that some visible tool/widget is not relevant anymore to current context of application.  It can be used when kind of tools should be rebuilt when some selections are changed.

Use following method to create instances:
	CmdToolContext for: aTool
	
Internal Representation and Key Implementation Points.

    Instance Variables
	tool:		<Object>
"
Class {
	#name : 'CmdToolContext',
	#superclass : 'Object',
	#instVars : [
		'tool'
	],
	#category : 'Commander-Core',
	#package : 'Commander-Core'
}

{ #category : 'instance creation' }
CmdToolContext class >> for: aTool [
	^self new
		tool: aTool
]

{ #category : 'testing' }
CmdToolContext >> allowsExecutionOf: aCommandClass [

	^aCommandClass canBeExecutedInContext: self
]

{ #category : 'command execution' }
CmdToolContext >> applyResultOf: aCommand [
	aCommand applyResultInContext: self
]

{ #category : 'command execution' }
CmdToolContext >> executeCommand: aCommand by: aCommandActivator [
	aCommand execute
]

{ #category : 'testing' }
CmdToolContext >> hasUnacceptedEdits [ 
	^ false 
]

{ #category : 'testing' }
CmdToolContext >> isSimilarTo: anotherToolContext [
	^self = anotherToolContext
]

{ #category : 'ui requests' }
CmdToolContext >> openContextMenu: aMenu [

	tool openContextMenu: aMenu
]

{ #category : 'command execution' }
CmdToolContext >> prepareFullExecutionOf: aCommand [
	aCommand prepareFullExecutionInContext: self
]

{ #category : 'command execution' }
CmdToolContext >> prepareNewCommand: aCommand [
	aCommand readParametersFromContext: self
]

{ #category : 'command execution' }
CmdToolContext >> processFailure: anException of: aCommand [
	anException notifyUserOfCommand: aCommand
]

{ #category : 'accessing' }
CmdToolContext >> tool [
	^ tool
]

{ #category : 'accessing' }
CmdToolContext >> tool: anObject [
	tool := anObject
]
