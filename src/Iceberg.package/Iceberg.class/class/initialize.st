class initialization
initialize
	| tree nodes |
	tree := SystemSettingsPersistence default settingTree.
	nodes := tree deeplySelect: [ :node | node receiverPackage name beginsWith: 'Iceberg' ].
	SystemSettingsPersistence updateSettingNodes: nodes.
