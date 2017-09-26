finalization
finalizeValues
	" cleanup the receiver, so it could be reused "
	| ex |
	ex := executor.
	executor := nil.
	next := nil.
	ex finalize.