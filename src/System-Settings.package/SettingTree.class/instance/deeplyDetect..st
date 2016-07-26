querying
deeplyDetect: aBlock 
	self settingTreeRoots
		do: [:sub | (sub deeplyDetect: aBlock)
				ifNotNil: [:found | ^ found]].
	^ nil 