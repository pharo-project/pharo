"
A WeakClassVariable is a class variable that references an object weakly.

Internally it is implemented with a weak array of size one. Writing into the variable writes the only field in the array. 

weakArray at: 1 put: aValue.

Reading into the variable reads the only field in the array.

weakArray at: 1
"
Class {
	#name : #WeakClassVariable,
	#superclass : #LiteralVariable,
	#category : #'VariablesLibrary-ClassVariables'
}

{ #category : #'code generation' }
WeakClassVariable >> emitStore: aMethodBuilder [
	"generate bytecode for 'varname at: 1 put: <stackTop>'"
	| temp |
	temp := '0slotTempForStackManipulation'.
	"Pop the value to store into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	aMethodBuilder popTop.
	
	"Push the weak array into the stack, then the arguments, then send"
	aMethodBuilder pushLiteralVariable: self.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #at:put:
]

{ #category : #'code generation' }
WeakClassVariable >> emitValue: aMethodBuilder [
	"Push the weak array at: 1 on the stack"
	aMethodBuilder pushLiteralVariable: self.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder send: #at:
]

{ #category : #initialization }
WeakClassVariable >> installingIn: aClass [
	"when the var gets installed, initialize it"
	super write: (WeakArray new: 1)
]

{ #category : #'meta-object-protocol' }
WeakClassVariable >> read [

	^ super read at: 1
]

{ #category : #'meta-object-protocol' }
WeakClassVariable >> write: aValue [

	^super read at: 1 put: aValue
]
