actions
debugSuite: aTestSuite
	self executeSuite: aTestSuite as: [ :aSuite | aSuite debug ].