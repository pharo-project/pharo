accessing
logging: boolean

	self primitiveSSL: handle setIntProperty: self constantsSQSSL_PROP_LOGLEVEL toValue: (boolean ifFalse: [ 0 ] ifTrue: [ 1 ]).
