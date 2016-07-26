testing
definesOrExtendsClass: aClass
	"Returns true whether the class, aClass, is one of the locally defined classes of the receiver or 
	if the receiver extends such class (that is defined in another package)"
	
	| canonizedName |
	canonizedName := aClass theNonMetaClass name.
	^ (classes includes: canonizedName) 
		or: [self extendedClassNames includes:  canonizedName]
