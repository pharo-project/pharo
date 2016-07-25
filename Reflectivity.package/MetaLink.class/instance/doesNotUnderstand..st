private
doesNotUnderstand: message

	(message selector isUnary and: [ message selector beginsWith: 'option'] )
		ifTrue: [ ^ options includes: message selector ].
		
	^ super doesNotUnderstand: message