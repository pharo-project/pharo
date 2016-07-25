private-formatting
formatMethodPatternFor: aMethodNode
	aMethodNode arguments isEmpty
		ifTrue: [ codeStream nextPutAll: aMethodNode selector ]
		ifFalse: [ self privateFormatMethodPatternMonoLineFor: aMethodNode ]