browser compatibility
setClass: aClass selector: aSelector
	| method protocol |
	method := aClass methodDict at: aSelector ifAbsent: [ nil ].
	protocol := method ifNil: [ nil ] ifNotNil: [ method protocol ].
	self
		selectedPackage: (self parentOfClass: aClass);
		selectedClass: aClass;
		showInstance: aClass isMeta not;
		selectedCategory: protocol;
		selectedMethod: method