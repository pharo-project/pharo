visiting
visitNode: aNode
	super visitNode: aNode.
	aNode hasMetalinkAfter ifTrue: [  
 		aNode afterHooks do: [:each | super visitNode: each]].