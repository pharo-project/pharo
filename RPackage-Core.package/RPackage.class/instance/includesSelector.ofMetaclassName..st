testing
includesSelector: aSelector ofMetaclassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"
	
	^ (self includesDefinedSelector: aSelector ofMetaclassName: aClassName)
		or: [(self includesExtensionSelector: aSelector ofMetaclassName: aClassName)]