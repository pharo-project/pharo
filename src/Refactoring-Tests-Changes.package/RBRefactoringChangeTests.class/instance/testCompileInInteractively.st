tests
testCompileInInteractively
	| change |
	change := RBAddMethodChange compile: 'setUp' in: self class classified: #running for: self.
	self assert: change controller = self.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #running.
	self universalTestFor: change