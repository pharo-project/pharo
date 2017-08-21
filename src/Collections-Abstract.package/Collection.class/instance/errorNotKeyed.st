private
errorNotKeyed

	self error: ('Instances of {1} do not respond to keyed accessing messages.' format: {self class name})
