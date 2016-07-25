setting
setRanks: collectionOfNodes at: aRank
	collectionOfNodes do: [ :aNode | aNode pathWeight: aRank ]
	