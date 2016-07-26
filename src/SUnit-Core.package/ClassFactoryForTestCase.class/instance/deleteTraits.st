cleaning
deleteTraits
	self createdTraits do: [:trait|
		self delete: trait]