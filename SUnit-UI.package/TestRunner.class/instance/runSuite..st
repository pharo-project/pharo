actions
runSuite: aTestSuite
	self executeSuite: aTestSuite as: [ :aSuite :aResult | aSuite run: aResult ].
	self updateResults

