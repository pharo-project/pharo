private
validateConnectionForHost: host port: port
	(connection isNil or: [ request url isNil ]) 
		ifTrue: [ ^ self ].
	(request url host = host and: [ request url port = port ])
		ifFalse: [ self close ]  