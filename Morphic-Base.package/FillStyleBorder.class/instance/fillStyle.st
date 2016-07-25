accessing
fillStyle
	"Answer the value of fillStyle"

	^fillStyle ifNil: [self color]