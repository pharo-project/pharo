querying
deeplySelect: aBlock in: aCollection 
	self settingTreeRoots
		do: [:aRoot | aRoot deeplySelect: aBlock in: aCollection]. 
	^ aCollection