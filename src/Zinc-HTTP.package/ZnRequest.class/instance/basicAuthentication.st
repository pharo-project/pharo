accessing
basicAuthentication
	| authorization separator |
	authorization := self authorization findTokens: ' '.
	(authorization size = 2 and: [ authorization first = 'Basic' ])
		ifFalse: [ ^ NotFound signal: 'Incomplete Basic Authentication' ].
	authorization := ZnUtils decodeBase64: authorization second.
	separator := authorization 
		indexOf: $: 
		ifAbsent: [ ^ NotFound signal: 'Incomplete Basic Authentication' ].
	^ { authorization first: separator - 1. authorization allButFirst: separator }