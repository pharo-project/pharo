wrapping
wrapItem: anItem index: index

	^ wrapSelector
		ifNil: [ anItem asString ]
		ifNotNil: [ 
			wrapSelector numArgs = 0
				ifTrue: [ anItem perform: wrapSelector ]
				ifFalse: [ self model 
								perform: wrapSelector 
								withEnoughArguments: {anItem. index. self.} ]]