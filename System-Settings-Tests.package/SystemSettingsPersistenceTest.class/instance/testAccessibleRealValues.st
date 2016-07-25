tests
testAccessibleRealValues
	"Ensure we can access all setting real values."
	self systemSettingNodeList do: [ :eachSettingNode |
		eachSettingNode settingDeclaration hasValue and: [ eachSettingNode settingDeclaration realValue ] ]