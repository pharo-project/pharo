private
extendedMethodsBecomeDefinedForMetaclass: aClassName
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"
	
	| ext |
	(metaclassExtensionSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := metaclassExtensionSelectors at: aClassName.
	metaclassExtensionSelectors removeKey: aClassName  ifAbsent: [self reportBogusBehaviorOf:  #extendedMethodsBecomeDefinedForMetaclass: ].
	(metaclassDefinedSelectors keys includes: aClassName) 
			ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	metaclassDefinedSelectors at: aClassName put: ext.