private
checkedAt: index
	index > self size ifTrue: [self error: 'not enough elements'].
	^ self at: index