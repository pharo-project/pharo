visiting
analyseForLinks: aNode
	| generator |
	aNode hasMetalink
		ifFalse: [ ^ self ].
	generator := HookGenerator node: aNode.

	aNode propertyAt: #preambles put: generator preamble.
	aNode preambles do: [:each | self visitNode: each].
	
	aNode hasMetalinkBefore ifTrue: [  
		aNode propertyAt: #beforeHooks put: generator beforeHooks.
 		aNode beforeHooks do: [:each | self visitNode: each]].
	
	aNode hasMetalinkAfter ifTrue: [  
		aNode propertyAt: #afterHooks put: generator afterHooks.
		self visitNode: aNode afterHooks].
	
	aNode hasMetalinkInstead ifTrue: [  
		aNode propertyAt: #insteadHooks put: generator insteadHooks.
 		aNode insteadHooks do: [:each | self visitNode: each]].	
