api - context
selectedMethod
	^selectorToUse 
		ifNotNil: [self selectedClass 
					compiledMethodAt: selectorToUse 
					ifAbsent: [self selectedClass class methodDict at: selectorToUse]]
 		ifNil:[nil].