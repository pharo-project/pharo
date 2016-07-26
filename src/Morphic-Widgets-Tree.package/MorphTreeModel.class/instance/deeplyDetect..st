accessing
deeplyDetect: aBlock 
	self rootNodes
		do: [:sub | (sub deeplyDetect: aBlock)
				ifNotNil: [:found | ^ found]].
	^ nil 