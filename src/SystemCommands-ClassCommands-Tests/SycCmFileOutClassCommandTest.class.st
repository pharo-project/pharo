Class {
	#name : 'SycCmFileOutClassCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycCmFileOutClassCommandTest >> testDefaultFileNameForMultipleClasses [
	| cmd context |
	cmd := SycCmFileOutClassCommand new.
	context := SycMockClassContext new.
	context selectedObjects: { Point . Rectangle }.
	cmd context: context.
	cmd prepareFullExecution.
	self assert: cmd defaultFileName equals: 'classfileout'
]

{ #category : 'tests' }
SycCmFileOutClassCommandTest >> testDefaultFileNameForSingleClass [
	| cmd context |
	cmd := SycCmFileOutClassCommand new.
	context := SycMockClassContext new.
	context selectedObjects: { Point }.
	cmd context: context.
	cmd prepareFullExecution.
	self assert: cmd defaultFileName equals: 'Point'
]
