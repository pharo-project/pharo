running
setUp
	super setUp.
	wrappers := ((packages := self packagesIn: suite)
		flatCollect: [ :package | self methodsIn: package ])
		collect: [ :each | HDTestCoverage on: each ].
	wrappers do: [ :each | each install ]