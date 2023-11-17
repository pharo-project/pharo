"
A MCClassInstanceVariableDefinition represents a class instance variable.
"
Class {
	#name : 'MCClassInstanceVariableDefinition',
	#superclass : 'MCVariableDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'accessing' }
MCClassInstanceVariableDefinition class >> type [
	^ #classInstance
]

{ #category : 'testing' }
MCClassInstanceVariableDefinition >> isClassInstanceVariable [
	^ true
]
