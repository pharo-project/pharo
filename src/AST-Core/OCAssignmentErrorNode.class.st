"
This is a particular englobing node that is an assigment.
Exemple : `indentifier := expression`
Can be created by forgetting the identifier `:= expression`.

If the identifier is present but the expression is missing, a valid ASTAssignmentNode is created but with a error node for its value.
"
Class {
	#name : 'OCAssignmentErrorNode',
	#superclass : 'OCEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}
