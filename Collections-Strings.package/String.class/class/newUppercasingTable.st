initialization
newUppercasingTable
	"a table for translating to upper case"
	^ String withAll: (Character allByteCharacters collect: [:c | c asUppercase]).