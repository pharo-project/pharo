testing
extendsClass: aClass
	"Returns true if the receiver extends aClass (that is defined in another package)"
	
	| canonizedName |
	canonizedName := aClass theNonMetaClass name.
	^ self extendedClassNames includes:  canonizedName