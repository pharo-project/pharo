initialize
createTempVectorNamed: name withVars: anArray
	
	"self addVectorTemps: anArray."
	self addTemp: name.
	self add: (IRInstruction createTempVectorNamed: name withVars: anArray).
