testing
includesExtensionSelector: aSelector ofMetaclassName: aClassName
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"
	
	| sels |
	sels := metaclassExtensionSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector.