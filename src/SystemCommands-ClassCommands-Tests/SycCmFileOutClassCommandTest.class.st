Class {
	#name : 'SycCmFileOutClassCommandTest',
	#superclass : 'SycAbstractCommandTest',
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
	self assert: cmd defaultFileName equals: 'classfileout'
]

{ #category : 'tests' }
SycCmFileOutClassCommandTest >> testDefaultFileNameForSingleClass [

	| cmd |
	cmd := SycCmFileOutClassCommand new.
	cmd context: self contextOnPoint.

	self assert: cmd defaultFileName equals: 'Point'
]
