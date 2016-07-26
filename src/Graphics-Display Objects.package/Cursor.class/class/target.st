constants
target
	"Answer the instance of me that is the shape of a gunsight."
	"Cursor target show"
	^TargetCursor ifNil:[self initTarget]