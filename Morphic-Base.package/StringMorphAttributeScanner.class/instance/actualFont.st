accessing
actualFont
	"Answer the value of actualFont"

	^ actualFont ifNil: [ TextStyle defaultFont ]