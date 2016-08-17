accessing
tempVariableReadNodes
		^self variableReadNodes select: [:each | each isTemp]