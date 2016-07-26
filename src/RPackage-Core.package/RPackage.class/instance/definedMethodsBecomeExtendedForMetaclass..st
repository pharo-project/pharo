private
definedMethodsBecomeExtendedForMetaclass: aClassName
	"the package may contain defined methods and their class is removed to the receiver. The status of such methods should now be extended"
	"Precondition: aClassName is in the defined list"
	
	| ext |
	(metaclassDefinedSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := metaclassDefinedSelectors at: aClassName.
	metaclassDefinedSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf: #definedMethodsBecomeExtendedForMetaclass:].
	(metaclassExtensionSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	metaclassDefinedSelectors at: aClassName put: ext.