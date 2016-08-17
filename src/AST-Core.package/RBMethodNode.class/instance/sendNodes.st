accessing
sendNodes
	^self allChildren select: [:each | each isMessage].