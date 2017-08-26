tests
testSettingDescription

	ReExceptionStrategy allSubclassesDo: [ :strategy |
		self assert: (
			ReExceptionStrategy settingDescription
				includesSubstring: strategy strategyDescription) ]