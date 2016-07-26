exceptionDescription
messageText
	^ String streamContents: [ :s | 
		selector 
			ifNotNil: [ s print: class; nextPutAll:  '>>'; print: selector; space]
			ifNil: [ s cr ].
		s nextPut: $(; print: name; nextPutAll: ' is Undeclared) ' ]