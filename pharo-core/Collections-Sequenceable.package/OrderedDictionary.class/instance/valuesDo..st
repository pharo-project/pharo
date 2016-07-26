enumerating
valuesDo: aBlock
	self keysDo: [:each | aBlock value: (self at: each)]