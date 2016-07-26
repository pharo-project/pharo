controlling
push: value 
	"Push value on the receiver's stack."

	self stackp: stackp + 1.
	self at: stackp put: value