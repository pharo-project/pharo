evaluating
value: anObject
	"Invoke the continuation and answer anObject as return value."

	self terminate: thisContext.
	self restoreValues.
	thisContext swapSender: values first.
	^ anObject