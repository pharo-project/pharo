accessing
instanceVariableReadNodes
		^self variableReadNodes select: [:each | each isInstance]