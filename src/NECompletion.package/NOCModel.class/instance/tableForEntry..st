accessing
tableForEntry: ent
	^ (ent notEmpty and: [ent first isUppercase]) 
			ifTrue: [classTable]
			ifFalse: [table]