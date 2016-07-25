removing elements
removePool: anORPoolDefinition
	"removes anORPoolDefinition from the pools dictionary if satisfies it #isPool"

	anORPoolDefinition isPool ifFalse:[ ^self ].
	self removeElement: anORPoolDefinition from: self pools