"
A class instance variable definition
"
Class {
	#name : 'RGClassInstanceVariableDefinition',
	#superclass : 'RGVariableDefinition',
	#category : 'Ring-Definitions-Core-Base',
	#package : 'Ring-Definitions-Core',
	#tag : 'Base'
}

{ #category : 'initialization' }
RGClassInstanceVariableDefinition >> initialize [

	super initialize.
	self isMeta: true
]

{ #category : 'testing' }
RGClassInstanceVariableDefinition >> isClassInstanceVariable [

	^true
]
