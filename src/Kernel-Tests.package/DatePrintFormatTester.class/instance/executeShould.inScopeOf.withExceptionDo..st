asserting
executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock

	^[aBlock value.
 	false] 
		on: anException
		do: [:exception | 
			anotherBlock value: exception.
			exception return: true]