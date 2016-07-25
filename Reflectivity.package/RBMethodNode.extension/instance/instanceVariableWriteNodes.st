*Reflectivity
instanceVariableWriteNodes
		^self variableWriteNodes select: [:each | each isInstance]