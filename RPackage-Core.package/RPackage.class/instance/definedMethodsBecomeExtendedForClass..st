private
definedMethodsBecomeExtendedForClass: aClassName
	"the package may contain defined methods and their class is removed to the receiver. The status of such methods should now be extended"
	"Precondition: aClassName is in the defined list"
	
	| ext |
	(classDefinedSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := classDefinedSelectors at: aClassName.
	classDefinedSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf:   #definedMethodsBecomeExtendedForClass: ].
	(classExtensionSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	classExtensionSelectors at: aClassName put: ext.