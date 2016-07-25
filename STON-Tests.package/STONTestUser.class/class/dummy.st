instance creation
dummy
	"self dummy"
	
	| username password |
	username := String streamContents: [ :stream |
		stream << 'user'; print: 999 atRandom; << '@company'; print: 99 atRandom; << '.com' ].
	password := String streamContents: [ :stream |
		stream << 'secret'; print: 999 atRandom ].
	^ self new
		username: username;
		password: password;
		yourself