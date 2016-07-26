private-replacing
replaceSourceWith: aNode 
	(self isContainmentReplacement: aNode) 
		ifTrue: [^self replaceContainmentSourceWith: aNode].
	aNode isMessage ifFalse: [^super replaceSourceWith: aNode].
	^self replaceSourceWithMessageNode: aNode