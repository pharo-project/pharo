testing
isInstalled
	self methodClass ifNotNil:
		[:class|
		self selector ifNotNil:
			[:selector|
			^self == (class compiledMethodAt: selector ifAbsent: [])]].
	^false