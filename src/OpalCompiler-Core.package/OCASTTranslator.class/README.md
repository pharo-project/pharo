I visit an abstract syntax tree and generate IR (intermediate representation) instructions for each node by sending the appropriate message to my methodBuilder (an IRBuilder).  I hold onto my two subclasses 
OCASTTranslatorForValue for generating instructions for effect and value, and
OCASTTranslatorForEffect for generating instructions for effect only.

Which one to use depends on the AST nodes and whether the code will only be executed (for effect only) or if the value is used afterwards (for value).

For example, when translating a return, the value to return needs to be pushed on stack, so the valueTranslator is used:
visitReturnNode: aReturnNode 
	valueTranslator visitNode: aReturnNode value.
	methodBuilder returnTop.
	
Whereas, in #visitMethodNode:,  the effectTranslator is used, because no value is pushed on stack at the end of the method body:
...effectTranslator visitNode: aMethodNode body..
