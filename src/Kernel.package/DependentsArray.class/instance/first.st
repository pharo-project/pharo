accessing
first
	self do: [:dep | ^dep].
	self error: 'this collection is empty'