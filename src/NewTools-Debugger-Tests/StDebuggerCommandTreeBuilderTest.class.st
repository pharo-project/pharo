Class {
	#name : #StDebuggerCommandTreeBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #running }
StDebuggerCommandTreeBuilderTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	
	super setUp.
	builder := StDebuggerMockCommandTreeBuilder new
]

{ #category : #tests }
StDebuggerCommandTreeBuilderTest >> testDisplayStrategy [
	self assert: builder displayStrategy class equals: builder class displayStrategy
]

{ #category : #tests }
StDebuggerCommandTreeBuilderTest >> testGroup [
	|rootGroup|
	rootGroup := (CmCommandGroup named: 'root') asSpecGroup.
	builder buildCommandsGroupWith: StDebugger basicNew forRoot: rootGroup.
	self assert: builder group identicalTo: rootGroup / builder class groupName.
	self assert: builder group name equals: builder class groupName.
	self assert: builder group description equals: builder class groupDescription
]

{ #category : #tests }
StDebuggerCommandTreeBuilderTest >> testGroupBuilding [
	|rootGroup|
	rootGroup := (CmCommandGroup named: 'root') asSpecGroup.
	builder buildCommandsGroupWith: StDebugger basicNew forRoot: rootGroup.
	builder buildAndRegisterGroup.
	self assert: rootGroup groups size equals: 1.
	self assert: rootGroup groups first identicalTo: rootGroup / builder class groupName.
	self assert: rootGroup groups first name equals: builder class groupName.
	self assert: rootGroup groups first description equals: builder class groupDescription.
	self assert: rootGroup groups first displayStrategy class identicalTo: builder class displayStrategy.
	self deny: rootGroup groups first isRoot
]

{ #category : #tests }
StDebuggerCommandTreeBuilderTest >> testGroupDescription [
	self assert: builder groupDescription equals: builder class groupDescription
]

{ #category : #tests }
StDebuggerCommandTreeBuilderTest >> testGroupName [
	self assert: builder groupName equals: builder class groupName
]
