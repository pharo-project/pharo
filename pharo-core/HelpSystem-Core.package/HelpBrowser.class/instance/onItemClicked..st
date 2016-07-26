events
onItemClicked: anItem
	anItem ifNil: [ ^ self sourceTextModel setText: rootTopic asHelpTopic contents ].
	self sourceTextModel setText: anItem contents