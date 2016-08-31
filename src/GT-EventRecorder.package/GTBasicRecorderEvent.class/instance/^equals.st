comparing
= otherEvent

	^ self class == otherEvent class
	and: [ (self info = otherEvent info)
	and: [ self timestamp = otherEvent timestamp ] ].