evaluating
value

	argumentNames := Set new.
	stream := WriteStream on: String new.
	self writeSourceCode.
	
	^ stream contents