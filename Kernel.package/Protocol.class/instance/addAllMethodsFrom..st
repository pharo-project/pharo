accessing
addAllMethodsFrom: aProtocol

	aProtocol methods do: [ :each | self addMethod: each ].