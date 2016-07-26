list
sortedKeys

	^ [ [ self object valuesAndCounts keys sorted ] 
		on: Error 
		do: [ "case of non sortable keys" self object valuesAndCounts keys ] 
			] on: Error do: [ #() ]