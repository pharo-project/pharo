variables
doRemotes
	| remotes |
	remotes := scope tempVars select: [ :each | each isEscapingWrite].
	remotes do: [ :each | scope moveToVectorTemp: each].	
	