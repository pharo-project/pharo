"
You are referencing a variable that is not declared anywhere. There is no temp, instance var, class var, or a global variable that can be bound to by this reference. Most likely you got into this state by writing an entirely correct code, but then the variable was removed.
"
Class {
	#name : #GRUndeclaredVariableRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules
}

{ #category : #running }
GRUndeclaredVariableRule >> basicCheck: aNode [
	^ aNode isVariable and: [ aNode isUndeclared ]
]

{ #category : #helpers }
GRUndeclaredVariableRule >> critiqueFor: aNode [
	^ (super critiqueFor: aNode)
		tinyHint: aNode name;
		yourself
]

{ #category : #accessing }
GRUndeclaredVariableRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
GRUndeclaredVariableRule >> name [
	^ 'References an undeclared variable'
]

{ #category : #accessing }
GRUndeclaredVariableRule >> severity [
	^ #error
]
