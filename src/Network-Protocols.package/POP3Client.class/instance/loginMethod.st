private
loginMethod
	^self connectionInfo at: #loginMethod ifAbsent: [nil]