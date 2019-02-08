"
I'm the superclass of the all the nodes for 
	- expressions and methods (subclasses of RBProgramNode) - historically it was the only tree
	- class definition (just nodes to represent the class creation messages),
	- full program (class definitions or method definitions)
	
I'm a new player in the field, do not hesitate to improve me.

"
Class {
	#name : #RBNode,
	#superclass : #Object,
	#category : #'AST-Core-Nodes'
}
