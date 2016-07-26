running
basicCheck: aClass 
	aClass isMeta ifTrue: [ ^ false ].
	^  (aClass classVarNames allSatisfy: [ :each | 
			each first isUppercase ]) not or: [  
		(aClass sharedPoolNames allSatisfy: [ :each | 
			each first isUppercase ]) not ]