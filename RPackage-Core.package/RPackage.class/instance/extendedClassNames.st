accessing
extendedClassNames 
	"Return the name of the classes which are extended by the receiver package. if a metaclass is extended, just get its sole instance class name."
	
	^ classExtensionSelectors keys union: metaclassExtensionSelectors keys