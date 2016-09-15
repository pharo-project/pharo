visiting
visitSend: send

	send superOf
		ifNil: [ gen send: send selector ]
		ifNotNil: [ :behavior |  gen send: send selector toSuperOf: behavior ]
