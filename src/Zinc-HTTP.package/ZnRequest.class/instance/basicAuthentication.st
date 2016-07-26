accessing
basicAuthentication
	| authorization username password |
	authorization := self authorization findTokens: ' '.
	(authorization size = 2 and: [ authorization first = 'Basic' ]) ifFalse: [ 
		^ NotFound signal: 'Incomplete Basic Authentication' ].
	authorization := (ZnUtils decodeBase64: authorization second) findTokens: ':'.
	authorization size = 2 ifFalse: [
		^ NotFound signal: 'Incomplete Basic Authentication' ].
	^ authorization