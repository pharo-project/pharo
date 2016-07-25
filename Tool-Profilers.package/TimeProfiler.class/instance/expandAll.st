actions
expandAll
	treeMorph expandAllSuchThat: [:node | node percentage > self minPercentageToExpand]