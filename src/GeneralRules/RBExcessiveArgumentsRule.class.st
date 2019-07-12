"
This smell arises when a method contains a long number of argument (five or more), which can indicate that a new object should be created to wrap the numerous parameters. The defined number of arguments can be edited in RBExcessiveArgumentsRule>>argumentsCount.

If the arguments are used in multiple methods this is a clear indication for the creation of an object representing the context in which the computation should occur. 
	
The defined number of arguments can be edited in #argumentsCount.
"
Class {
	#name : #RBExcessiveArgumentsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBExcessiveArgumentsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBExcessiveArgumentsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ExcessiveArgumentsRule'
]

{ #category : #private }
RBExcessiveArgumentsRule >> argumentsCount [
	^ 5
]

{ #category : #running }
RBExcessiveArgumentsRule >> basicCheck: aMethod [
	^ aMethod numArgs >= self argumentsCount
]

{ #category : #accessing }
RBExcessiveArgumentsRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBExcessiveArgumentsRule >> name [
	^ 'Excessive number of arguments'
]

{ #category : #accessing }
RBExcessiveArgumentsRule >> rationale [
	^ 'Long argument lists (five or more) can indicate that a new object should be created to wrap the numerous parameters.'
]
