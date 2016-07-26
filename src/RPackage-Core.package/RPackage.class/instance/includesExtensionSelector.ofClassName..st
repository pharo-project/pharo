testing
includesExtensionSelector: aSelector ofClassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"
	
	| sels |
	sels := classExtensionSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector.