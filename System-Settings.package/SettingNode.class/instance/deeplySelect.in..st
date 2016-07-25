menu
deeplySelect: aBlock in: aCollection
	(aBlock value: self) ifTrue: [aCollection add: self].
	self contents do: [:sub | sub deeplySelect: aBlock in: aCollection].
	^ aCollection