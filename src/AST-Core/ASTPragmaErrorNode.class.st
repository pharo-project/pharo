"
This is a particular englobing node that is a pragma.
Exemple : < pragma node >
Can be created by forgetting 
the closure : < pragma node .
Forgetting the opening will be assumed to be a binary selector.
"
Class {
	#name : 'ASTPragmaErrorNode',
	#superclass : 'ASTEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
ASTPragmaErrorNode class >> error: aToken withNodes: aCollection [
	"A RBPragmaErrorNode can only be created from the opening token by choice.
	 Since the closure is a superior sign, the default assumption is that it's a binary selector."
	"If the collection is empty, there is only the token in the error."
	^aCollection isEmpty
		ifTrue: [ self new contents: aCollection; start: aToken start; stop: aToken stop; errorMessage: '''>'' expected' ]
		ifFalse: [ self new contents: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: '''>'' expected' ]
]

{ #category : 'class initialization' }
ASTPragmaErrorNode class >> initialize [

	self deprecatedAliases: { #ASTPragmaErrorNode }
]

{ #category : 'testing' }
ASTPragmaErrorNode >> isPragmaError [
	^true
]

{ #category : 'testing' }
ASTPragmaErrorNode >> isPrimitive [
	^false
]
