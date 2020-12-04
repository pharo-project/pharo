"
You are referencing a variable that is not declared anywhere. There is no temp, instance var, class var, or a global variable that can be bound to by this reference. Most likely you got into this state by writing an entirely correct code, but then the variable was removed.
"
Class {
	#name : #ReUndeclaredVariableRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #running }
ReUndeclaredVariableRule >> basicCheck: aNode [
	^ aNode isVariable and: [ aNode isUndeclaredVariable ]
]

{ #category : #helpers }
ReUndeclaredVariableRule >> critiqueFor: aNode [
	^ (super critiqueFor: aNode)
		tinyHint: aNode name;
		yourself
]

{ #category : #accessing }
ReUndeclaredVariableRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReUndeclaredVariableRule >> name [
	^ 'References an undeclared variable'
]

{ #category : #accessing }
ReUndeclaredVariableRule >> severity [
	^ #error
]
