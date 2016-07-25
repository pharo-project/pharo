building suites
buildSuiteFromMethods: testMethods

	^testMethods
		inject: (self suiteClass named: self name asString)
		into: [:suite :selector |
			suite
				addTest: (self selector: selector);
				yourself]