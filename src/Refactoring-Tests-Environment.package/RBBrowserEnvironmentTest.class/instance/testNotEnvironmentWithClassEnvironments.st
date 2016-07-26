testing-environments
testNotEnvironmentWithClassEnvironments
	| numberEnvironment notNumberEnvironment |
	numberEnvironment := (RBBrowserEnvironment new) forClasses: Number withAllSubclasses.
	notNumberEnvironment := numberEnvironment not.

	self universalTestFor: notNumberEnvironment.
	self deny: (notNumberEnvironment includesClass: Number).
	self assert: (notNumberEnvironment & numberEnvironment) classes isEmpty.
	self assert: (notNumberEnvironment not includesClass: Number)