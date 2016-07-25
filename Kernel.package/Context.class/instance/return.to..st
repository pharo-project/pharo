controlling
return: value to: aSender
	"Simulate the return of value to aSender."

	self releaseTo: aSender.
	aSender ifNil: [^ nil].
	^ aSender push: value