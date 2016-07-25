running
runCase
	self resources do: [:each | each availableFor: self].
	[self setUp.
	self performTest] ensure: [
		self tearDown.
		self cleanUpInstanceVariables]