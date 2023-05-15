"
This is a particular englobing node that is an assigment.
Exemple : `indentifier := expression`
Can be created by forgetting the identifier `:= expression`.

If the identifier is present but the expression is missing, a valid RBAssignmentNode is created but with a error node for its value.
"
Class {
	#name : #RBAssignmentErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}
