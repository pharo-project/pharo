options
prepareRequestHook
	"Return the current request preparation hook, an object conforming 
	to the #value: protocol that gets the final chance to change a request 
	right before it gets executed. Defaults to nil.
	Typically used to sign requests. See also #prepareRequest: "

	^ self optionAt: #prepareRequestHook ifAbsent: [ nil ]