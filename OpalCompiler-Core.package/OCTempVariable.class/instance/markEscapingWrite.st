escaping
markEscapingWrite
	escaping := #escapingWrite.
	self isRepeatedWrite ifFalse:[usage := #write]