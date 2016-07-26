transforming
movePoolVariables
	pools 
		do: [:poolDict | toClasses do: [:each | self movePool: poolDict toClass: each]]