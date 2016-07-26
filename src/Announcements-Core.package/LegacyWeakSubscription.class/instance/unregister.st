finalization
unregister

	self subscriber ifNotNil: [:sub |  self weakRegistry remove: sub ]
		