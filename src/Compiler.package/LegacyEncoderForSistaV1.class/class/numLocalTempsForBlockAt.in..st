block closure support
numLocalTempsForBlockAt: startpc in: method
	| firstByte |
	firstByte := method at: startpc.
	^ firstByte = 230 
		ifFalse: [ 0 ]
		ifTrue: [ (method at: startpc + 1) ]
	