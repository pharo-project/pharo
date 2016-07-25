printing
printOn: stream
	stream
		nextPutAll: self main;
		nextPut: $/;
		nextPutAll: self sub.
	parameters isNil ifFalse: [ 
		parameters keysAndValuesDo: [ :key :value | 
			stream
				nextPut: $;;
				nextPutAll: key;
				nextPut: $=;
				nextPutAll: value ] ]