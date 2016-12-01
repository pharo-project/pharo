This class models a scope for the workspace (and the debugger and all other tools).

The default scope of the compiler is initialized with a Requestor scope, if the requestor is not nil (see CompilationContext>>scope)

The OCRequestorScope will ask the tool (the requestor) for bindings. This will be an association, and as such it will create a OCLiteralVariable.  It will compile the same bytecode as for a global, but it will use the associations hold on by the tool to do so.