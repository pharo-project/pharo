testing
ifNil: nilBlock ifNotNil: ifNotNilBlock
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock cull: self