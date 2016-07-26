testing
includesExtensionSelector: aSelector ofClass: aClass
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"
	
	| sels |
	sels := self extensionSelectorsForClass: aClass.
	^ sels includes: aSelector.