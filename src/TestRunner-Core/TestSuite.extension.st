Extension { #name : 'TestSuite' }

{ #category : '*TestRunner-Core' }
TestSuite >> runBy: aRunner [

	aRunner runSuite: self
]
