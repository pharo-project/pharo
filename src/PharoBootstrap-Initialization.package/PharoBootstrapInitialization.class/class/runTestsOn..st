running - tests
runTestsOn: aPackageName
	| result packageTags suite mcVersion |
	mcVersion := $- join: (aPackageName substrings: '-') allButLast. "remove the author and extension part".
	packageTags := (SystemOrganization categories 
		select: [ :categoryName | categoryName beginsWith: mcVersion ]) flattened.
	suite := (Smalltalk at: #TestSuite) named: mcVersion.
	packageTags
		do: [ :categoryName |
			(self testClassesFrom: categoryName)
				reject: [ :testCase | self ignoredTests includes: testCase name ]
    			thenDo: [ :testCase | testCase addToSuiteFromSelectors: suite ].
		].
	result := suite run.
	result errors
		do: [ :error |
			'Test failed: ' logCr.
			error logCr
		]
