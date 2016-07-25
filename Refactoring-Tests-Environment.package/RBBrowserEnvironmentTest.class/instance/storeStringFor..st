private
storeStringFor: aBrowserEnvironment
	| newEnvironment |
	newEnvironment := Smalltalk compiler evaluate: aBrowserEnvironment storeString.
	self assert: newEnvironment numberSelectors = aBrowserEnvironment numberSelectors.
	self assert: (newEnvironment not & aBrowserEnvironment) numberSelectors = 0
	