startup - shutdown
runList: aList do: aBlock
	aList do: [ :each | 
		[aBlock value: each] 
			on: Exception
			do: [ :error | self errorHandler handleError: error] ]