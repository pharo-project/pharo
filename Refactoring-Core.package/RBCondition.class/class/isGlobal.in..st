instance creation
isGlobal: aString in: aRBSmalltalk 
	^self new 
		type: (Array with: #isGlobal with: aString)
		block: [aRBSmalltalk includesGlobal: aString asSymbol]
		errorString: aString , ' is <1?:not >a class or global variable'