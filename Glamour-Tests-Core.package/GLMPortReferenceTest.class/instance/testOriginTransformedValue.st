tests
testOriginTransformedValue
	| port reference |
	port := GLMSimplePort new value: 41.
	reference := (GLMOriginPortReference new)
		port: port;
		transformation: [ :x | x + 1 ].
	self assert: reference value equals: 42