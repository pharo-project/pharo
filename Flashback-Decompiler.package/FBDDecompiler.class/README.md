I am the main class of FBD package.
My job is to decompile a compiledMethod to get valid Abstract Syntax Tree (AST).

The first step of decompilation is to give the byte code to the FBDLoopScanner, to annotate it. This will be important to detect all the loops in the method and handle it correctly in the Decompiler.

The second step of decompilation is to interpret the byte code to create corresponding AST nodes. The decompiler will call the right ASTBuilder method and create a full AST. 

Then the AST is returned. The Flashback Decompiler's job stops there, because there are already some frameworks to generate Smalltalk code from an AST.

The returned AST is does not take optimized messages into account. Use FBDOptimizdMessagesRewriter to recover the optimized messages.

Instance Variables
	simulatedStack <OrderedCollection> Simulates the bytecode stack, pushing AST nodes instead of concrete values
	builder <FBDASTBuilder> change to another builder to build another AST than RB. By default, use a builder building RB nodes 
	instructionStream <InstructionStream> used to decode the bytecode.
	currentSequence <RBSequence> current AST sequence being decompiled
	argCount <SmallInteger> counter to create args into the current sequence with a valid name
	tempCount <SmallInteger> counter to create temps into the current sequence with a valid name
	jumpSize <SmallInteger> used to remember a jump size during a dual branch message decompilation
	loopsArray <Array> is the array containing informations about loops that the loop scanner gave me
