tests
testNonExistingStoredSettingVariable
	<expectedFailure>
	reader stream: MockSettings changedClassStoredSettingVariable readStream.
	reader load.
	self assert: SettingsStonReader new storedSettings isNil