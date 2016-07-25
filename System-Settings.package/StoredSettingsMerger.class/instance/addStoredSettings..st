adding
addStoredSettings: aCollectionOfStoredSettings
	aCollectionOfStoredSettings do: [ :eachStoredSetting |
		self addStoredSetting: eachStoredSetting ].
	^ aCollectionOfStoredSettings