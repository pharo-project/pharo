running
reset

	self nodes do: [:n|
		n pathWeight: Float infinity ].
	self edges do: [:e |
		e visited: false ]