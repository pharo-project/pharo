accessing
rootItem
	^ rootItem ifNil: [ rootItem := self createRootItem ]