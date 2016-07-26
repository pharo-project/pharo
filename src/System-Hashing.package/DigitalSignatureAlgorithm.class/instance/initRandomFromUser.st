initialization
initRandomFromUser
	"Ask the user to type a long random string and use the result to seed the secure random number generator."

	| s |
	s := UIManager default request: 'Enter a long random string to seed the random generator.'.
	s ifNil: [s := '']. 
	^self initRandomFromString: s