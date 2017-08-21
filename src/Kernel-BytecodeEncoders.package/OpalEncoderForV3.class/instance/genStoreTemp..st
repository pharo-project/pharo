bytecode generation
genStoreTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63