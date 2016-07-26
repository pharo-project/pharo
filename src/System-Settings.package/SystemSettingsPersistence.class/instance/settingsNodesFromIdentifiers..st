enumerating
settingsNodesFromIdentifiers: aCollection 
	^ aCollection 
			collect: [ :eachIdentifier |
				self settingTree deeplyDetect: [ :eachSettingNode |
					eachSettingNode settingNodeIdentifier = eachIdentifier ] ]
			thenReject: #isNil