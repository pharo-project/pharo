instance variables
allInstVarNames

	^self allInstanceVariables collect:[ :ivar| ivar name ]