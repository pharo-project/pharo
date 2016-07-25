typing/selecting keys
selectAll: aKeyboardEvent 
	"select everything"

	self closeTypeIn.
	self selectFrom: 1 to: self string size.
	^ true