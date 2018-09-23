Extension { #name : #BehaviorTest }

{ #category : #'*Kernel-Tests-Extended' }
BehaviorTest >> testAllLocalCallsOn [
	
	self assert: (( Point allLocalCallsOn: #asPoint )  notEmpty).
	self assert: (( Point allLocalCallsOn: #asPoint )  size = 4).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point>> #roundDownTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundUpTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #truncateTo: ) asRingDefinition).
	
]

{ #category : #'*Kernel-Tests-Extended' }
BehaviorTest >> testWhichSelectorsAccess [
	self assert: ((Point whichSelectorsAccess: 'x') includes: #x).
	self deny:  ((Point whichSelectorsAccess: 'y') includes: #x).
]
