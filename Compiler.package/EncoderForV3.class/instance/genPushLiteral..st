bytecode generation
genPushLiteral: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal Constant #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 128 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 96;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255