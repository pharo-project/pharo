examples
defineCopiedVarBecomeDeadContext
	| a b |
	b := [ a := 1.a := a + 1. (DebuggerMethodMapOpal forMethod: thisContext method)  tempNamed:'a' in:thisContext].
	^ b