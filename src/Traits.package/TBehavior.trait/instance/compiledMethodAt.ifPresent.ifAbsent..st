accessing method dictionary
compiledMethodAt: selector ifPresent: anotherBlock ifAbsent: aBlock
	"Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock"

	^ self methodDict at: selector ifPresent: anotherBlock ifAbsent: aBlock