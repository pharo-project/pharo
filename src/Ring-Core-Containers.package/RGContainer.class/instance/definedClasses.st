accessing
definedClasses
	"Retrieves classes (traits are included)"

	^self elements at: #definedClasses ifAbsentPut:[ IdentityDictionary new ]