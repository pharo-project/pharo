convenient accesses
extensionMethods
	"Retrieves a collection (by default a dictionary) with the extensions methods of the receiver"

	^self methods values select:[ :mth| mth isExtension ]