enumerating
associationsDo: aBlock
	self keysDo: [:each | aBlock value: (self associationAt: each)]