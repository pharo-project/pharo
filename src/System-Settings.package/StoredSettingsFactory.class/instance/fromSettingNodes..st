building
fromSettingNodes: aCollectionOfSettingNodes 
	storedSettings := OrderedCollection new.
	aCollectionOfSettingNodes do: [ :eachSettingNode |
		(self from: eachSettingNode) ifNotNil: [ :aStoredNode | storedSettings add: aStoredNode ] ].
	^ storedSettings