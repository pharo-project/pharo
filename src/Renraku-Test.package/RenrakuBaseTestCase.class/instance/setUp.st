running
setUp
	super setUp.
	testPackage := (RPackage named: #'Renraku-Nuclear-Site') register.
	self setupScreamerRule.