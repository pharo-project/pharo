testing
includesSelector: aSelector ofClassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"
	
	^ (self includesDefinedSelector: aSelector ofClassName: aClassName)
		or: [(self includesExtensionSelector: aSelector ofClassName: aClassName)]