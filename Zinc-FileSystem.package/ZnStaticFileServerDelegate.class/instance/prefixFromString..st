accessing
prefixFromString: string
	"Set the path prefix under which I should be serving files to string,
	interpreting each /-separated token as a path element"

	self prefix: (string findTokens: '/')