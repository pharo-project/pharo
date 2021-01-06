Class {
	#name : #StDebuggerCodeCommandTreeBuilderTest,
	#superclass : #StDebuggerToolCommandTreeBuilderTest,
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #'instance creation' }
StDebuggerCodeCommandTreeBuilderTest >> newBuilder [
	^ StDebuggerCodeCommandTreeBuilder
		buildCommandsGroupWith: debugger
		forRoot: rootGroup
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testCodeDebugCommands [
	|codeCommandClasses|
	codeCommandClasses := OrderedCollection new.
	codeCommandClasses addAll: builder codeExecutionControlCommandsClasses.
	self
		assertCommandGroup: builder codeDebugCommands
		hasSameCommands: codeCommandClasses
		withContext: debugger
		displayStrategy: CmUIDisplayAsGroup
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testCodeDebugCommandsDescription [
	self assert: builder  codeDebugCommandsDescription equals: 'Execution control'
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testCodeDebugCommandsGroupName [
	self assert: builder  codeDebugCommandsGroupName equals: 'Execution'
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testCodeExecutionControlCommandsClasses [
	self
		assertCollection: builder codeExecutionControlCommandsClasses
		equals: {StRunToSelectionCommand}
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testDisplayStrategy [
	self assert: builder class displayStrategy equals: CmUIDisplayAsSubMenu
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testGroupDescription [
	self assert: builder class groupDescription equals: 'Debug actions from code'
]

{ #category : #tests }
StDebuggerCodeCommandTreeBuilderTest >> testGroupName [
	self assert: builder class groupName equals: 'Code'
]
