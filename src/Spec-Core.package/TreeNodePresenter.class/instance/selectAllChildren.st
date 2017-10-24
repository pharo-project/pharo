api
selectAllChildren

	self children value do: [ :e | 
		e selected: true.
		e selectAllChildren ].