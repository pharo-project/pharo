running
run
	| result |
	result := self classForTestResult new.
	[ result runCase: self]
		ensure: [ self classForTestResource resetResources: self resources ].
	^ result