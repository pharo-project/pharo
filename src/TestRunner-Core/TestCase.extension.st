Extension { #name : 'TestCase' }

{ #category : '*TestRunner-Core' }
TestCase >> runBy: aTestRunner [

	aTestRunner runCase: self
]
