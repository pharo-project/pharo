running
initializeNodes

	self nodes do: [:n|
		n auth: 1.0; hub: 1.0
	]