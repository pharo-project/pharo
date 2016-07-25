updating screen
exposed: exposeTime
	(lastExposeTime notNil and: [ exposeTime <= lastExposeTime]) ifTrue: [ ^ self ].
	lastExposeTime := exposeTime.
	self exposed