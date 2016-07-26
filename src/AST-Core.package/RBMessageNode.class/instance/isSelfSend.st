testing
isSelfSend
	^ self receiver isVariable and: [ self receiver name = 'self' ]