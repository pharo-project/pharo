private
with: aString
	"Answer a relative path of the given string. N.B. that the argument is not parsed; 
	it is the name of a single path element, and path separators in it do not have 
	special meaning."
	
	"(Path with: '/parent/child/') isRelative
	answers true because this is a relative path to a file or directory named 
	'/parent/child/'. In bash: \/parent\/child\/"

	| inst |
	inst := self new: 1.
	inst at: 1 put: aString.
	^ inst