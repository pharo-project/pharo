block closure support
numLocalTempsForBlockAt: startpc in: method
	"4 = size of push closure copy bytecode"
	^ BlockLocalTempCounter
		tempCountForBlockAt: startpc - 4 
		in: method 