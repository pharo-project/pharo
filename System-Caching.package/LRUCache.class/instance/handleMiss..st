private
handleMiss: association
	| link |
	statistics addMiss.
	self addWeight: association value.
	link := lruList addLast: association.
	keyIndex at: association key put: link.
	^ association value