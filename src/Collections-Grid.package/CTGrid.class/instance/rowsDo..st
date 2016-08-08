enumerating
rowsDo: aBlock

	self rows do: [ :aRow | aBlock value: aRow ]