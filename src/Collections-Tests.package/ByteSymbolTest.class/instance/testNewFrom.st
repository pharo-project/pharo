tests - creation
testNewFrom
	"self debug: #testNewFrom"

| dt newFrom1 newFrom2 |

dt := DateAndTime now asString .
newFrom1 := ByteSymbol newFrom: dt .

self assert:
	(Symbol allSymbols select: [:e | e asString  = dt  ]  ) size = 1 .
self assert:
	(ByteSymbol allInstances select: [:e | e = dt  ] ) size = 1 .
self assert:
	(Symbol allSymbols select: [:e | e asString  = dt  ]  ) =
			(ByteSymbol allInstances select: [:e | e = dt ] ) .
			
newFrom2 := ByteSymbol newFrom: dt . 
self assert:
	(Symbol allSymbols select: [:e | e asString  = dt  ]  ) size = 1 .
self assert:
	(ByteSymbol allInstances select: [:e | e = dt ] ) size = 1 .
	