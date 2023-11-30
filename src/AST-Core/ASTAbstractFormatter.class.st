"
I am the abstract class to find the formatters in the system.
"
Class {
	#name : 'ASTAbstractFormatter',
	#superclass : 'ASTProgramNodeVisitor',
	#category : 'AST-Core-Formatter',
	#package : 'AST-Core',
	#tag : 'Formatter'
}

{ #category : 'class initialization' }
ASTAbstractFormatter class >> initialize [

	self deprecatedAliases: { #ASTAbstractFormatter }
]
