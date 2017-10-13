bytecode generation
genPushNClosureTemps: numTemps
	numTemps timesRepeat: [ self genPushSpecialLiteral: nil ]