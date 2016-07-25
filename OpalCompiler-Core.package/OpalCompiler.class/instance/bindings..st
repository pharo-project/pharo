accessing
bindings: aDictionary
	"for now we abuse the requestor infrastructure...we should rewrite the requestor to use bindings
	instead"
	self compilationContext 
		requestor: aDictionary;
		interactive: false.