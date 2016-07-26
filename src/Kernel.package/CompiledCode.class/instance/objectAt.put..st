literals
objectAt: index put: value 
	"Primitive. Store the value argument into a literal in the receiver. An 
	index of 2 corresponds to the first literal. Fails if the index is less than 2 
	or greater than the number of literals. Answer the value as the result. 
	Normally only the compiler sends this message, because only the 
	compiler stores values in CompiledMethods. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed