instance creation
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the
	argument, value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)