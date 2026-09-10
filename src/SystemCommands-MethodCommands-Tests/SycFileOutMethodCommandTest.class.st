Class {
	#name : 'SycFileOutMethodCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycFileOutMethodCommandTest >> testDefaultFileNameForEmptyMethods [
	| cmd context |
	cmd := SycFileOutMethodCommand new.
	context := SycMockMethodContext new.
	context selectedMethods: #(  ).
	cmd context: context.
	self assert: cmd defaultFileName equals: 'methodsfileout'
]

{ #category : 'tests' }
SycFileOutMethodCommandTest >> testDefaultFileNameForMultipleMethods [
	| cmd context |
	cmd := SycFileOutMethodCommand new.
	context := SycMockMethodContext new.
	context selectedMethods: { Point >> #x . Point >> #y }.
	cmd context: context.
	self assert: cmd defaultFileName equals: 'methodsfileout'
]

{ #category : 'tests' }
SycFileOutMethodCommandTest >> testDefaultFileNameForSingleBinaryMethod [
	| cmd context |
	cmd := SycFileOutMethodCommand new.
	context := SycMockMethodContext new.
	context selectedMethods: { Point >> #+ }.
	cmd context: context.
	self assert: cmd defaultFileName equals: 'Point-+'
]

{ #category : 'tests' }
SycFileOutMethodCommandTest >> testDefaultFileNameForSingleKeywordMethod [
	| cmd context |
	cmd := SycFileOutMethodCommand new.
	context := SycMockMethodContext new.
	context selectedMethods: { Point >> #setX:setY: }.
	cmd context: context.
	self assert: cmd defaultFileName equals: 'Point-setX:setY:'
]

{ #category : 'tests' }
SycFileOutMethodCommandTest >> testDefaultFileNameForSingleUnaryMethod [
	| cmd context |
	cmd := SycFileOutMethodCommand new.
	context := SycMockMethodContext new.
	context selectedMethods: { Point >> #x }.
	cmd context: context.
	self assert: cmd defaultFileName equals: 'Point-x'
]
