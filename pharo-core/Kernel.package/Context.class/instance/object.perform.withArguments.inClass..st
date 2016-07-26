mirror primitives
object: anObject perform: selector withArguments: argArray inClass: lookupClass
	"Send the selector, aSymbol, to anObject with arguments in argArray.
	 Fail if the number of arguments expected by the selector 
	 does not match the size of argArray, or if lookupClass
	 cannot be found among the anObject's superclasses.
	 Primitive. Essential for the debugger."

	<primitive: 100 error: error>
	(selector isSymbol) ifFalse:
		[^self error: 'selector argument must be a Symbol'].
	(argArray isMemberOf: Array) ifFalse:
		[^self error: 'argArray must be an Array'].
	(selector numArgs = argArray size)
		ifFalse: [^self error: 'incorrect number of arguments'].
	((self objectClass: anObject) == lookupClass
	 or: [(self objectClass: anObject) inheritsFrom: lookupClass]) ifFalse:
		[^self error: 'lookupClass is not in anObject''s inheritance chain'].
	self primitiveFailed