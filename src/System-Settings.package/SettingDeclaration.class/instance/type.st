accessing
type
	"Answer the value of type"

	^ type ifNil: [type := self realValue class name]