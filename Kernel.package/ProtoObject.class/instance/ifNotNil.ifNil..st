testing
ifNotNil: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am not nil, so evaluate the block ifNotNilBlock"

	^ ifNotNilBlock cull: self