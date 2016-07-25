method changes
methodChangesAtClass: className
	"Return an old-style dictionary of method change types."

	^(changeRecords at: className ifAbsent: [^ Dictionary new])
		methodChangeTypes