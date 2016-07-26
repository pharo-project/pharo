private-register
removeAllMethodsFromClassNamed: aClassNameSymbol
	"Remove all the methods (defined and extensions) that are related to class aClassNameSymbol. aClassNameSymbol is the name of a class and not a metaclass - see class comment."

	classDefinedSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	classExtensionSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	metaclassDefinedSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	metaclassExtensionSelectors removeKey: aClassNameSymbol ifAbsent: [nil].