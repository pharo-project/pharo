loading
updateSettingNode: aSettingNode 
	| value |
	[ value := self realValue ] on: Error do: [ :exception | "ignore and leave" 
		self inform: 'Cannot read stored value of ', self settingNodeIdentifier, '. Exception: ', exception printString.
		^ self ].
	[ aSettingNode realValue: value ] on: Error do: [ :exception | "inform and ignore"
		self inform: 'Cannot update ', self settingNodeIdentifier, '. Exception: ', exception printString ]