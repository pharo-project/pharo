Handles the byte code generation of jumps, literals and similar things. Byte code independent. The encoder is bytecode dependent.

I generate bytecodes in response to 'instructions' messages being sent to me.  I rewrite jumps at the end so their jump offsets are correct (see #bytecodes).  For example, to create a compiled method that compares first instVar to first arg and returns 'yes' or 'no' (same example as in IRBuilder), do:

	BytecodeGenerator new
		numArgs: 1;
		pushInstVar: 1;
		pushTemp: 1;
		send: #>;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod

You can send #ir to the compiledMethod to decompile to its IRMethod, and you can send #methodNode to either to decompile to its parse tree.


Instance Variables
	additionalLiterals:		<OCLiteralSet> Those are literals evaluated for effect and optimized control flow messages selectors used in the method in order to be able to do sendersOf: on these literals with success.
	bytes:		<OrderedCollection of bytes> current sequence of bytecodes being written 
	encoder:		<BytecodeEncoder> Set the one you need depending on which bytecode set you want to use.
	forceLongForm:		<Boolean> true if instance variable access requires a long form (typically, Context access)
		
	Following inst vars are used to map bytecode pc to IR instruction	
	instrMap:		<OrderedCollection>
	instrMaps:		<IdentityDictionary of OrderedCollection>
			
	lastLiteral:		<Behavior> Basically in the case you would compile without annotating the method with its class and its selector, this literal is used to enforce the method class to be a literal to allow super sends.
	lastSpecialReturn:		<Message> used to generate quick returns
	literals:		<OCLiteralList> literals of the methods
	numArgs:		<Smi> number of arguments of the method
	numberOfTemps:		<Smi> number of temps of the method
	primNumber:		<Smi> primitive number (or 0)
	properties:		<AdditionalMethodState | nil> used to hold the additional method state (pragmas for examples)
			
	Following inst vars are used to correctly map the jumps (See #initialize for extra information)
	orderSeq:		<OrderedCollection> Reverse map of seq order
	seqBytes:		<IdentityDictionary>
	seqOrder:		<IdentityDictionary>
	jumps:		<IdentityDictionary>
	currentSeqId:		<Object>
	currentSeqNum:		<Smi>
			
	stack:		<Stack> simulated stack. Only simulates the depth of the stack. Store its maximum value in stacks instance variable.
	stacks:		<IdentityDictionary (seqId -> stackCount)> used to find out the maximum depth of the method and therefore set the largeFrameBit