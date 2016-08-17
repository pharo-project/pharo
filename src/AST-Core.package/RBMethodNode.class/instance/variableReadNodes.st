accessing
variableReadNodes
	^self variableNodes select: [:each | each isRead]