running
run
	| result |
	result := self resultClass new.
	self run: result.
	^result