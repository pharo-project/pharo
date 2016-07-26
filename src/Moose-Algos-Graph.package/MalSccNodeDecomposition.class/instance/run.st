running
run

	self nodes do: [:n|
		self run: n
	]