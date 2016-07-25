data flow instructions
send: selector super: supered numArgs: numberArguments
	supered 
		ifTrue:[ self superSend: selector numArgs: numberArguments ]
		ifFalse: [ self send: selector numArgs: numberArguments ]