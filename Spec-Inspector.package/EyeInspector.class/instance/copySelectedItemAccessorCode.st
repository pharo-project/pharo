actions
copySelectedItemAccessorCode
	self hasSelectedObject
		ifFalse: [ ^ self ].
	self selectedElement copyAccessorCode.