instance creation
empty
	"Returns an empty condition"

	^self new 
		type: (Array with: #empty)
		block: [true]
		errorString: 'Empty'