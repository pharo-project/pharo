tests
testAccessRealValues
	"Ensure we can access all setting real values."
	self systemSettingNodeList do: [ :eachSettingNode | 
				eachSettingNode item hasValue and: [ eachSettingNode item realValue ] ].