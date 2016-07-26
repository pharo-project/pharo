private
errorMacro
	^errorMacro isNil
		ifTrue: [self longMacro]
		ifFalse: [super errorMacro]