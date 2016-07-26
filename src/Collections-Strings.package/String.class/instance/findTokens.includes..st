accessing
findTokens: delimiters includes: substring
	"Divide self into pieces using delimiters.  Return the piece that includes substring anywhere in it.  Is case sensitive (say asLowercase to everything beforehand to make insensitive)."

^ (self findTokens: delimiters) 
	detect: [:str | (str includesSubstring: substring)] 
	ifNone: [nil]