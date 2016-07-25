options
bindingAddress
	"Return the interface address we are (or will be) listening on.
	Nil means that we are (or will be) listening on all/any interfaces."
	
	^ self optionAt: #bindAddress ifAbsent: [ nil ] 