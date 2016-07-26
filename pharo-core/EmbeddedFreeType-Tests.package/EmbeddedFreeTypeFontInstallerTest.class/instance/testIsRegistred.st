tests
testIsRegistred
	| installer provider |
	installer := self currentInstallerOrError.
	provider := self currentProviderOrError.
	self assert: (provider includesInstaller: installer)