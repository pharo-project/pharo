tests - creation
testAs
	"self debug: #testAs"

| tStr tAs1 tAs2 |

tStr := DateAndTime now asString .
tAs1 :=  tStr as: ByteSymbol . 
self assert: 	(Symbol allSymbols select: [:e | e asString  = tStr ]  ) size = 1 .
self assert: 	(ByteSymbol allInstances select: [:e | e asString = tStr] ) size = 1 .  
self assert: 	(ByteSymbol allInstances select: [:e | e asString = tStr] ) =
		(Symbol allSymbols select: [:e | e asString  = tStr ]  ) .
		
tAs2 :=  tStr as: ByteSymbol . 
self assert: 	(Symbol allSymbols select: [:e | e asString  = tStr ]  ) size = 1 .
self assert: 	(ByteSymbol allInstances select: [:e | e asString = tStr] ) size = 1 .  

