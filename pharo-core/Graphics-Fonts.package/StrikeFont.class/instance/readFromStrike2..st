file in/out
readFromStrike2: fileName 
	"StrikeFont new readFromStrike2: 'Palatino14.sf2'"
	"Build an instance from the strike font stored in strike2 format.
	fileName is of the form: <family name><pointSize>.sf2"
	| file |
	self assert:['*.sf2' match: fileName]. "likely incompatible"
	name := fileName copyUpTo: $..	"Drop filename extension"
	file := FileStream readOnlyFileNamed: fileName.
	file binary.
	[ self readFromStrike2Stream: file ] ensure: [ file close ]