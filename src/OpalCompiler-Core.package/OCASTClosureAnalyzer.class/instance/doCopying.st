variables
doCopying
	| copying |
	copying := scope tempVars select: [ :each | each isEscapingRead].	
	copying do: [ :each | scope addCopyingTemp: each]