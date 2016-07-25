testing
includesDefinedSelector: aSelector ofMetaclassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"
	
	| sels |
	sels := metaclassDefinedSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector.