private
errorNotKeyed

	self error: ('Instances of {1} do not respond to keyed accessing messages.' translated format: {self class name})
