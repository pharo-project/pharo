tests parsing
testParserErrors
	#(#('self foo. + 3' 11) #('#(' 3) #('self 0' 6) #('self asdf;;asfd' 11)) 
		do: [:each | self parseError: each]