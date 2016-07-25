parsing
parseArguments
"
Parse a arguments spec in a form of: 

#( type1 name1 , type2 name2 , nil, true, false , 100 )
"
	[ stream atEnd ] whileFalse: [
		self parseArgument ifNotNil: [:arg | 
			fnSpec addArgument: arg].
		
		self skipSpace.
		
		stream atEnd ifFalse: [ stream peek = $, ifFalse: [ self error: 'comma expected' ].
			stream next. self skipSpace. ]
	].