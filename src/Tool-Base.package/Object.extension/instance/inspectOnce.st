*Tool-Base
inspectOnce
	"Inspect unless we have already done it once."
	
	| node |
	node := thisContext sender sourceNodeExecuted.
	(node hasProperty: #Once) ifTrue: [^self]. 
	node propertyAt: #Once put: true.
	^self inspect