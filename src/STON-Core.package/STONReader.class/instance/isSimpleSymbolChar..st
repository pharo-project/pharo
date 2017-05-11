private
isSimpleSymbolChar: char
	^char isLetter or: ['0123456789-_./' includes: char]