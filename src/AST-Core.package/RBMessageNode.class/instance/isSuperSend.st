testing
isSuperSend
	^ self receiver isVariable and: [ self receiver name = 'super' ]