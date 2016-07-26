menu
deeplyDetect: aBlock 
	(aBlock value: self) ifTrue: [^ self].
	self contents do: [:sub | (sub deeplyDetect: aBlock) ifNotNil: [:found | ^ found]].
	^ nil