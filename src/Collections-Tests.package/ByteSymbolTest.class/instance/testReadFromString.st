tests - creation
testReadFromString
	"self debug: #testReadFromString"

| str strWithPound readFrom1 readFrom2 |
Smalltalk garbageCollect. 
str := 'notYetExisting' .
self assert: (Symbol allSymbols select: [:e | e asString  = str ]  ) size = 0 .
self assert: (ByteSymbol allInstances select: [:e | e asString = str] ) size = 0 .  
strWithPound := ('#' , str) .
readFrom1 := ByteSymbol readFromString: strWithPound .
self assert:	(Symbol allSymbols select: [:e | e asString  = str  ]  ) size = 1 .
self assert:	(ByteSymbol allInstances select: [:e | e = str ] ) size = 1 .
self assert:	(Symbol allSymbols select: [:e | e asString  = str  ]  ) =
			(ByteSymbol allInstances select: [:e | e = str ] ) .
			
readFrom2  := ByteSymbol readFromString: strWithPound .
self assert:
	(Symbol allSymbols select: [:e | e asString  = str  ]  ) size = 1 .
self assert:
	(ByteSymbol allInstances select: [:e | e = str ] ) size = 1 .