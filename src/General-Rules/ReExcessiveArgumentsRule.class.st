"
This smell arises when a method contains a long number of argument (five or more), which can indicate that a new object should be created to wrap the numerous parameters. The defined number of arguments can be edited in RBExcessiveArgumentsRule>>argumentsCount.

If the arguments are used in multiple methods this is a clear indication for the creation of an object representing the context in which the computation should occur. 
	
The defined number of arguments can be edited in #argumentsCount.
"
Class {
	#name : 'ReExcessiveArgumentsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReExcessiveArgumentsRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReExcessiveArgumentsRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReExcessiveArgumentsRule class >> rationale [
	^ 'Long argument lists (five or more) can indicate that a new object should be created to wrap the numerous parameters.'
]

{ #category : 'accessing' }
ReExcessiveArgumentsRule class >> ruleName [
	^ 'Excessive number of arguments'
]

{ #category : 'accessing' }
ReExcessiveArgumentsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ExcessiveArgumentsRule'
]

{ #category : 'private' }
ReExcessiveArgumentsRule >> argumentsCount [
	^ 5
]

{ #category : 'running' }
ReExcessiveArgumentsRule >> basicCheck: aMethod [

	"If method includes UFFI calls then it is OK to have high number of arguments"
	aMethod isFFIMethod ifTrue: [ ^false ].

	^ aMethod numArgs >= self argumentsCount
]
