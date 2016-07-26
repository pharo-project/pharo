testing
includesDefinedSelector: aSelector ofClassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"
	
	| sels |
	sels := classDefinedSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector.