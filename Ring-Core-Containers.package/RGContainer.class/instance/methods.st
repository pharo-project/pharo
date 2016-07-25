accessing
methods
	"Retrieves methods defined in the container"
	
	^self elements at: #methods ifAbsentPut:[ IdentityDictionary new ]