private
numberSelectorsFor: aBrowserEnvironment
	| numberSelectors numberSelectorsNot |
	numberSelectors := aBrowserEnvironment numberSelectors.
	numberSelectorsNot := aBrowserEnvironment not numberSelectors.
	self assert: numberSelectors + numberSelectorsNot = universalEnvironment numberSelectors.
	self assert: (aBrowserEnvironment & aBrowserEnvironment not) numberSelectors = 0.
	self assert: (universalEnvironment & aBrowserEnvironment) numberSelectors = numberSelectors.
	self assert: (aBrowserEnvironment & universalEnvironment) numberSelectors = numberSelectors