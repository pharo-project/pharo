Class {
	#name : 'SycFileOutClassCommandTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycFileOutClassCommandTest >> testDefaultFileNameForMultipleClasses [
	| cmd context |
	cmd := SycFileOutClassCommand new.
	context := SycMockClassContext new.
	context selectedObjects: { Point . Rectangle }.
	cmd context: context.
	self assert: cmd defaultFileName equals: 'classfileout'
]

{ #category : 'tests' }
SycFileOutClassCommandTest >> testDefaultFileNameForSingleClass [

	| cmd |
	cmd := SycFileOutClassCommand new.
	cmd context: self contextOnPoint.

	self assert: cmd defaultFileName equals: 'Point'
]
