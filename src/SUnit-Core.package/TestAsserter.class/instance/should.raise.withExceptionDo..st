asserting
should: aBlock raise: anException withExceptionDo: anotherBlock 

	^self assert: (self executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock)