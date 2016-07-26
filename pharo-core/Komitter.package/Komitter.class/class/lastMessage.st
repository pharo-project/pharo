protocol
lastMessage

	^ lastMessage ifNil: [ MCSaveVersionDialog previousMessages first ]