bytecode generation
genPushInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 64;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255