I am an abtract superclass for compiler plugings.

The compiler consists of multiple passes:

source 
	- [ Scanner/Parser ]  -> 
AST 
	- [ SemCheck ] -> 
AST   <<HERE>>
	- [ ASTTranslator ] -> 
IR 
	- [ IRBuilder ] ->
 CompiledMethod


These plugins are called <<HERE>>, that is, after semantic analysis before generating the IR.
They are sorted by #priority and handed a *copy* of the AST.


