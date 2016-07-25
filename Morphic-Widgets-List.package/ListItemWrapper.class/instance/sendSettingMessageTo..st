dropping/grabbing
sendSettingMessageTo: aModel

	aModel 
		perform: (self settingSelector ifNil: [^self])
		with: self withoutListWrapper
