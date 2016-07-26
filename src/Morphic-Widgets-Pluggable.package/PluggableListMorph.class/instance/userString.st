debug and other
userString
	^list ifNotNil: [ 
		String streamContents: [:strm |
			list do: [:i |
				strm nextPutAll: (i string); cr
			]
		]
	]