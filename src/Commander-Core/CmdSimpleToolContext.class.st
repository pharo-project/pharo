"
I represent context of simple tool where tool itself provide required and complete information about own state.
I pass tool as context instance to command activation methods. For example:

	aCommand prepareFullExecutionInContext: tool.
	
I simplify command activation for tools when context is not really needed.

But it is not recomended to use me for tool with list widgets because in that case I do not keep actual selection state. And every my instance will represent same state which is not appropriate in many cases. For example drag and drop operation of list items requires real context reification.
"
Class {
	#name : #CmdSimpleToolContext,
	#superclass : #CmdToolContext,
	#category : #'Commander-Core'
}

{ #category : #testing }
CmdSimpleToolContext >> allowsExecutionOf: aCommandClass [

	^aCommandClass canBeExecutedInContext: tool
]

{ #category : #'command execution' }
CmdSimpleToolContext >> applyResultOf: aCommand [
	aCommand applyResultInContext: tool
]

{ #category : #'command execution' }
CmdSimpleToolContext >> prepareFullExecutionOf: aCommand [
	aCommand prepareFullExecutionInContext: tool
]

{ #category : #'command execution' }
CmdSimpleToolContext >> prepareNewCommand: aCommand [
	aCommand readParametersFromContext: tool
]

{ #category : #testing }
CmdSimpleToolContext >> representsSimpleAnnotationUser: aToolClass [

	^tool isKindOf: aToolClass
]
