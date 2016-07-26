testing
includesDefinedSelector: aSelector ofClass: aClass
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined in this package"
	
	| sels |
	sels := self definedSelectorsForClass: aClass.
	^ sels includes: aSelector.