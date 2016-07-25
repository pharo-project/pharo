private
errorMacro
	^errorMacro isNil
		ifTrue: ['unknown']
		ifFalse: [errorMacro]