visiting
visitSend: send

	send superOf
		ifNil: [ stream nextPutAll: 'send: '.
			send selector printOn: stream. ]
		ifNotNil: [ :behavior | stream nextPutAll: 'send: '.
			send selector printOn: stream.
			stream nextPutAll: ' toSuperOf: '.
			behavior printOn: stream ]

