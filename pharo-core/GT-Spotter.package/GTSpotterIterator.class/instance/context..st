accessing
context: aContext
	" we assume that if the context changes, so will the result of the iterator - so we reset the internal collection "
	context := aContext.
	self reset.