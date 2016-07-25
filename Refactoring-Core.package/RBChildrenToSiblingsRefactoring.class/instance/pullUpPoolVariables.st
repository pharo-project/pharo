private-variables
pullUpPoolVariables
	"Don't remove the pool variables from the subclass since they might be referenced there."

	| newSuperclass |
	newSuperclass := self abstractSuperclass.
	parent sharedPoolNames 
		do: [:each | newSuperclass addPoolDictionary: each]