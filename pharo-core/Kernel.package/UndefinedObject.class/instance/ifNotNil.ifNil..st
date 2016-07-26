testing
ifNotNil: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am nil, so evaluate the block nilBlock"

	^ nilBlock value