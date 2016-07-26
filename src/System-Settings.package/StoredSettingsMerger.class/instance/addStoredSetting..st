adding
addStoredSetting: aSettingNode 
	self storedSettings withIndexDo: [ :eachStoredNode :index | 
		eachStoredNode = aSettingNode ifTrue: [ ^ self storedSettings at: index put: aSettingNode ] ].
	^ self storedSettings add: aSettingNode