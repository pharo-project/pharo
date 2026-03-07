Extension { #name : 'Package' }

{ #category : '*TestRunner-Core' }
Package >> runBy: aTestRunner [

	aTestRunner runPackage: self
]

{ #category : '*TestRunner-Core' }
Package >> suite [

	| suite |
	suite := TestSuite new.
	(self definedClasses select: #isTestCase thenCollect: #suite) do: [ :aSuite | 
		suite addTests: aSuite tests ].
	suite name: self name.
	^ suite
]
