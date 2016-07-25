private
rebuildEnumAccessors
	self enumerationDictionary keysDo: [ :each | 
		self defineEnumAccesorFor: each ]