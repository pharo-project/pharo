running - tests
testClassesFrom: aCategoryName
	^ (SystemOrganization listAtCategoryNamed: aCategoryName)
				collect: [ :className | Smalltalk globals at: className ]
				thenSelect: [ :className |
					(className includesBehavior: (Smalltalk globals at: #TestCase))
						and: [ className isAbstract not ] ]