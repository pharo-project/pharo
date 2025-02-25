"
Checks for instance variables that might better be defined as temporary variables. If an instance variable is only used in one method and it is always assigned before it is used, then that method could define that variable as a temporary variable of the method instead (assuming that the method is not recursive).
"
Class {
	#name : 'ReVariableReferencedOnceRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReVariableReferencedOnceRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReVariableReferencedOnceRule class >> group [
	^ self designFlawGroup
]

{ #category : 'accessing' }
ReVariableReferencedOnceRule class >> ruleName [
	^ 'Variable referenced in only one method and always assigned first'
]

{ #category : 'accessing' }
ReVariableReferencedOnceRule class >> severity [
	^ #information
]

{ #category : 'accessing' }
ReVariableReferencedOnceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'VariableReferencedOnceRule'
]

{ #category : 'running' }
ReVariableReferencedOnceRule >> check: aClass forCritiquesDo: aCriticBlock [
		aClass slots do: [ :slot |
		  | usingMethods |
		  usingMethods := slot usingMethods.
		  usingMethods size = 1 ifFalse:  [ ^ self ].
		  (OCReadBeforeWrittenTester
				  isVariable: slot name
				  writtenBeforeReadIn: usingMethods first ast) ifTrue: [aCriticBlock cull: (self critiqueFor: aClass about: slot  name)   ]]
]
