html
escape: string
	"Write string, escaping characters as needed for regular text"
	
	string do: [ :each | self escapeCharacter: each ]