I am an instruction in the IR (intermediate representation) language.  The IR serves as the intermediary between the Smalltalk language and the bytecode language.  It is easier to optimize and translate to/from this language than it is to optimize/translate directly from Smalltalk to bytecodes.  The IR is generic and simple consisting of just twelve instructions.  They are:

	goto: labelNum
	if: boolean goto: labelNum1 otherwise: labelNum2
	label: labelNum
	popTop
	pushDup
	pushLiteral: object
	pushTemp: name
	blockReturn
	returnTop
	send: selector
	send: selector toSuperOf: behavior
	storeTemp: name

Each instruction is reified as an instance of one of my subclasses and grouped by basic block (IRSequence) into an IRMethod.  IRInterpreter visits each instruction in a IRMethod responding to the above instruction messages sent to it.
