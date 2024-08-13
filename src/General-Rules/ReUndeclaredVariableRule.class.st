"
You are referencing a variable that is not declared anywhere. There is no temp, instance var, class var, or a global variable that can be bound to by this reference. Most likely you got into this state by writing an entirely correct code, but then the variable was removed.
"
Class {
	#name : 'ReUndeclaredVariableRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReUndeclaredVariableRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReUndeclaredVariableRule class >> ruleName [
	^ 'References an undeclared variable'
]

{ #category : 'accessing' }
ReUndeclaredVariableRule class >> severity [
	^ #error
]

{ #category : 'running' }
ReUndeclaredVariableRule >> basicCheck: aNode [
	^ aNode isVariable and: [ aNode isUndeclaredVariable ]
]

{ #category : 'helpers' }
ReUndeclaredVariableRule >> critiqueFor: aNode [
	^ (super critiqueFor: aNode)
		tinyHint: aNode name;
		yourself
]
