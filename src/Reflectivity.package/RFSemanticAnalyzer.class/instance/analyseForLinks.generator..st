visiting
analyseForLinks: aNode generator: aGenerator

	aNode propertyAt: #preambles put: aGenerator preamble.
	aNode preambles do: [:each | self visitNode: each].
	
	aNode hasMetalinkBefore ifTrue: [  
		aNode propertyAt: #beforeHooks put: aGenerator beforeHooks.
 		aNode beforeHooks do: [:each | self visitNode: each]].
	
	aNode hasMetalinkAfter ifTrue: [  
		aNode propertyAt: #afterHooks put: aGenerator afterHooks.
		self visitNode: aNode afterHooks].
	
	aNode hasMetalinkInstead ifTrue: [  
		aNode propertyAt: #insteadHooks put: aGenerator insteadHooks.
 		aNode insteadHooks do: [:each | self visitNode: each]].	
