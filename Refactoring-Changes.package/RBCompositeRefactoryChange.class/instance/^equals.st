comparing
= aRefactoryBuilder 
	self class = aRefactoryBuilder class
		ifFalse: [ ^ false ].
	changes size = aRefactoryBuilder changes size
		ifFalse: [ ^ false ].
	changes with: aRefactoryBuilder changes do: [ :first :second |
		first = second
			ifFalse: [ ^ false ] ].
	^ true