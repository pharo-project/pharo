Extension { #name : #RBCondition }

{ #category : #'*SmartSuggestions' }
RBCondition class >> hasInstanceVariable: aClass [ 
	^self new 
		type: (Array with: aClass)
		block: [aClass allInstVarNames isEmpty not]
		errorString: aClass printString 
				, ' <1?:does not >define any instance variable ' , aClass asString 

]
