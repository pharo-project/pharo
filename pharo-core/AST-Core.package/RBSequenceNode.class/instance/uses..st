testing
uses: aNode 
	statements isEmpty ifTrue: [^false].
	aNode == statements last ifFalse: [^false].
	^self isUsed