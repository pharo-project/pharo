accessing
packages
	"Retrieves the children packages defined in the receiver"

	^self elements at: #packages ifAbsentPut:[ IdentityDictionary new ]