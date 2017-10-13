api
unselectAllChildren

	self children value do: [ :e | 
		e selected: false.
		e unselectAllChildren ].	