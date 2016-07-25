adding elements
addPool: anORPoolDefinition
	"adds anORPoolDefinition to the pools dictionary if satisfies it #isPool"

	anORPoolDefinition isPool ifFalse:[ ^self ].
	self addElement: anORPoolDefinition in: self pools