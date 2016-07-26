printing
settingNodeIdentifier
	^ String streamContents: [ :aStream |
			self settingNodeIdentifierOn: aStream ]