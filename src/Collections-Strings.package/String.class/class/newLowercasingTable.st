initialization
newLowercasingTable
	"a table for translating to lower case"
	^ String withAll: (Character allByteCharacters collect: [:c | c asLowercase]).