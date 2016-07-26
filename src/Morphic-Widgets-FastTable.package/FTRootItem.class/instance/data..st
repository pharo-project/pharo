accessing
data: aCollection
	self children: (aCollection collect: [ :object | FTBasicItem data: object from: dataSource ]) asArray