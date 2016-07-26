tests-packing
testNewAnnouncementFor
	announcement := packing newAnnouncementFor: collector.
	self assert: announcement serializedBy equals: packing class name.
	self assert: announcement eventRecorderVersion equals: packing version.
	self assert: announcement category equals: collector category.