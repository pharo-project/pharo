private
extendedMethodsBecomeDefinedForClass: aClassName
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"
	
	| ext |
	(classExtensionSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := classExtensionSelectors at: aClassName.
	classExtensionSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf:   #extendedMethodsBecomeDefinedForClass: ].
	(classDefinedSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	classDefinedSelectors at: aClassName put: ext.