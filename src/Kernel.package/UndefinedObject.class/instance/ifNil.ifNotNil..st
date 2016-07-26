testing
ifNil: nilBlock ifNotNil: ifNotNilBlock
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value