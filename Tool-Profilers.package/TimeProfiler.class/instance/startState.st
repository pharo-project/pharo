UI specific
startState
	treeMorph expandAllSuchThat: [:node | node percentage > self startMinPercentageToExpand]
	