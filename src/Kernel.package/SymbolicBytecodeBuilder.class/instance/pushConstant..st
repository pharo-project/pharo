instruction decoding
pushConstant: obj
	"Print the Push Constant, obj, on Top Of Stack bytecode."

	self addBytecode: (String streamContents:
				[:s |
				s nextPutAll: 'pushConstant: '.
				(obj isKindOf: LookupKey)
					ifFalse: [obj printOn: s]
					ifTrue: [obj key
						ifNotNil: [s nextPutAll: '##'; nextPutAll: obj key]
						ifNil: [s nextPutAll: '###'; nextPutAll: obj value soleInstance name]]]).
