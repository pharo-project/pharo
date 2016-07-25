accessing
commandKeyHandler
	"Answer the receiver's commandKeyHandler"

	^ self valueOfProperty: #commandKeyHandler ifAbsent: [nil]