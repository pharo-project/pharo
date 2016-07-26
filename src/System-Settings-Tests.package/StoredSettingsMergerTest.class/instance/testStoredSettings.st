tests
testStoredSettings
	| numberOfNodeSettings |
	numberOfNodeSettings := MockSettings nodeSettings size.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfNodeSettings.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfNodeSettings.
