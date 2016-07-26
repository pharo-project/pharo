*System-Support
referencedClasses
	"Return classes that are directly referenced by this method"

	^ self literals allButLast
		select: [ :l | l value isClass ] 
		thenCollect: [:v | v value ].