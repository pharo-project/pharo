utils
logColored: anAnsiiColorCode during: aBlock
	
	Smalltalk logStdErrorDuring: [:stderr |
		stderr
			nextPut: Character escape; 
			nextPut: $[; nextPutAll: anAnsiiColorCode; nextPut: $m.
		
		aBlock value: stderr.
		
		stderr nextPut: Character escape; nextPutAll: '[0m'
	]