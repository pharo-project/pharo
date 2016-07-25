*Reflectivity
variableWriteNodes
		^self variableNodes select: [:each | each isWrite]