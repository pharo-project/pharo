tests
testReadFrom

	self expectedBehavior do: [:association |
		self assert: (association key value == association value)]