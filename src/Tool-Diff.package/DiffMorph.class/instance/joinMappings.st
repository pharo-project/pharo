accessing
joinMappings
	"Answer the join parameters between src and dst."

	^joinMappings ifNil: [self calculateJoinMappings]