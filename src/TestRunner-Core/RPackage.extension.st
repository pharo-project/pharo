Extension { #name : 'RPackage' }

{ #category : '*TestRunner-Core' }
RPackage >> runBy: aTestRunner [

	aTestRunner runPackage: self
]
