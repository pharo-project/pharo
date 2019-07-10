Extension { #name : #ClassDescriptionTest }

{ #category : #'*Kernel-Tests-Extended' }
ClassDescriptionTest >> testAllLocalCallsOn [
	
	self assert: (( Point allLocalCallsOn: #asPoint )  notEmpty).
	self assert: (( Point allLocalCallsOn: #asPoint )  size) equals: 4.
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point>> #roundDownTo:) ).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundUpTo:) ).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundTo:) ).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #truncateTo: ) ).
]
